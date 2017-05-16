/*
 * TODO:
 *	- TK_NEQUAL
 *	- Use yyltype for lines and position? Makes it slower...
 *	- Remove ';' occasionally
 *	- Type of monitor initializer return for function in ast
 */

%{
	#include <stdlib.h>

	#include "ast.h"
	#include "errs.h"
	#include "scanner.h"

	// Auxiliary macro to use with lists
	#define APPEND(type, assignable, list, elem);	\
		if (!list) {								\
			assignable = elem;						\
		} else {									\
			type* e;								\
			for (e = list; e->next; e = e->next);	\
			e->next = elem;							\
			assignable = list;						\
		}											\

	// Auxiliary macro to wrap a list of declarations in the
	// appropriate Body or Block type
	#define WRAP_VARIABLE_DECLARATIONS(assignable, func, first, type)	\
		assignable = func(first);										\
		for (type* t = assignable; first->next; first = first->next) {	\
			t->next = func(first->next);								\
			t = t->next;												\
		}																\

	static void yyerror(const char* err);
%}

// Semantic information
%union {
	// Tokens
	int ival;
	double fval;
	const char* strval;

	// Nonterminals
	Body* body;
	Declaration* declaration;
	Definition* definition;
	Id* id;
	Type* type;
	Block* block;
	Statement* statement;
	Variable* variable;
	Expression* expression;
	FunctionCall* function_call;
}

// Tokens
%token <ival>
	'{' '}' '[' ']' '(' ')' '=' ';'
	TK_VALUE TK_VARIABLE TK_FUNCTION TK_DEFINE TK_WHILE TK_WAIT TK_IN TK_SIGNAL
	TK_BROADCAST TK_RETURN TK_IF TK_ELSE TK_FOR TK_SPAWN TK_TRUE TK_FALSE
	TK_MONITOR TK_PRIVATE TK_INITIALIZER

%token <ival> TK_INTEGER
%token <fval> TK_FLOAT
%token <strval> TK_STRING
%token <id> TK_LOWER_ID TK_UPPER_ID

// Nonterminals
%type <declaration>
	variable_declaration variable_declaration_base variable_declaration_single
	parameter_list parameters parameter lower_ids
%type <definition>
	variable_definition block_variable_definition
	variable_definition_value variable_definition_variable
	variable_definition_base
	function_definition method_definition constructor_definition
	monitor_definition
%type <type>
	type
%type <block>
	block block_content_list block_content else
%type <body>
	body_list body class_body class_content_list class_content
%type <statement>
	statement simple_statement compound_statement
%type <variable>
	variable
%type <expression>
	expression primary_expression literal argument_list arguments
%type <function_call>
	function_call

// Operator precedence and associativity
%left		<ival> TK_OR
%left		<ival> TK_AND
%nonassoc	<ival> TK_EQUAL TK_NEQUAL
%nonassoc	<ival> '<' '>' TK_LEQUAL TK_GEQUAL
%left		<ival> '+' '-'
%left		<ival> '*' '/'
%left		<ival> TK_NOT // precedence for unary minus

// First nonterminal
%start program

%%

// ==================================================
//
//	Program
//
// ==================================================

program
	: body_list
		{
			program = ast_program(ast_body($1));
		}
	;

body_list
	: /* empty */
		{
			$$ = NULL;
		}
	| body_list body
		{
			APPEND(Body, $$, $1, $2);
		}
	;

body
	: variable_definition_value ';'
		{
			$$ = ast_body_definition($1);
		}
	| function_definition
		{
			$$ = ast_body_definition($1);
		}
	| monitor_definition
		{
			$$ = ast_body_definition($1);
		}
	;

// ==================================================
//
//	Declaration
//
// ==================================================

variable_declaration_single
	: TK_LOWER_ID ':' type
		{
			Variable* variable = ast_variable_id($1);
			variable->type = $3;
			$$ = ast_declaration_variable(variable);
		}
	;

variable_declaration_base
	: variable_declaration_single
		{
			$$ = $1;
		}
	/*
		Syntactic sugar:

		a, b, c: Integer		->		a: Integer
								->		b: Integer
								->		c: Integer
	*/
	| lower_ids ',' variable_declaration_single
		{
			Declaration* last = NULL;
			for ($$ = $1; $1; last = $1, $1 = $1->next) {
				$1->variable->type = $3->variable->type;
			}
			last->next = $3; // there is always at least one lower_id
		}
	; 

variable_declaration
	: TK_VARIABLE variable_declaration_base
		{
			$2->variable->value = false;
			$$ = $2;
		}
	;

// ==================================================
//
//	Definition
//
// ==================================================

variable_definition_base
	: variable_declaration_single '=' expression
		{
			$$ = ast_definition_variable($1, $3);
		}
	| TK_LOWER_ID '=' expression
		{
			Variable* var = ast_variable_id($1);
			$$ = ast_definition_variable(ast_declaration_variable(var), $3);
		}
	;

variable_definition_value
	: TK_VALUE variable_definition_base
		{
			$2->variable.declaration->variable->value = true;
			$$ = $2;
		}
	;

variable_definition_variable
	: TK_VARIABLE variable_definition_base
		{
			$2->variable.declaration->variable->value = false;
			$$ = $2;
		}
	;

variable_definition
	: variable_definition_value
		{
			$$ = $1;
		}
	| variable_definition_variable
		{
			$$ = $1;
		}
	;

function_definition
	: TK_FUNCTION TK_LOWER_ID parameter_list ':' type block
		{
			Declaration* declaration = ast_declaration_function($2, $3, $5);
			$$ = ast_definition_function(declaration, $6);
		}
	| TK_FUNCTION TK_LOWER_ID parameter_list block
		{
			Declaration* declaration = ast_declaration_function($2, $3, NULL);
			$$ = ast_definition_function(declaration, $4);
		}
	;

monitor_definition
	: TK_MONITOR TK_UPPER_ID class_body
		{
			$$ = ast_definition_type(ast_type_monitor($2, $3));
		}
	;

// ==================================================
//
//	Type
//
// ==================================================

type
	: TK_UPPER_ID
		{
			$$ = ast_type_id($1);
		}
	| '[' type ']'
		{
			$$ = ast_type_array($2);
		}
	;

// ==================================================
//
//	Block
//
// ==================================================

block
	: '{' block_content_list '}'
		{
			$$ = ast_block($2);
		}
	;

block_content_list
	: /* empty */
		{
			$$ = NULL;
		}
	| block_content_list block_content
		{
			APPEND(Block, $$, $1, $2);
		}
	;

block_content
	: variable_declaration ';'
		{
			WRAP_VARIABLE_DECLARATIONS($$, ast_block_declaration, $1, Block);
		}
	| block_variable_definition ';'
		{
			$$ = ast_block_definition($1);
		}
	| statement
		{
			$$ = ast_block_statement($1);
		}
	;

// ==================================================
//
//	Statement
//
// ==================================================

statement
	: simple_statement ';'
		{
			$$ = $1;
		}
	| compound_statement
		{
			$$ = $1;
		}
	;

simple_statement
	: variable '=' expression
		{
			$$ = ast_statement_assignment($1, $3);
		}
	| function_call
		{
			$$ = ast_statement_function_call($1);
		}
	| TK_WHILE expression TK_WAIT TK_IN variable
		{
			$$ = ast_statement_while_wait($2, $5);
		}
	| TK_SIGNAL variable
		{
			$$ = ast_statement_signal($2);
		}
	| TK_BROADCAST variable
		{
			$$ = ast_statement_broadcast($2);
		}
	| TK_RETURN
		{
			$$ = ast_statement_return(NULL);
		}
	| TK_RETURN expression
		{
			$$ = ast_statement_return($2);
		}
	;

compound_statement
	: TK_IF expression block else
		{
			$$ = ($4)
				? ast_statement_if_else($2, $3, $4)
				: ast_statement_if($2, $3)
				;
		}
	| TK_WHILE expression block
		{
			$$ = ast_statement_while($2, $3);
		}
	/* | TK_FOR [?] ';' expression ';' [?] block */
	| TK_SPAWN block
		{
			$$ = ast_statement_spawn($2);
		}
	| block
		{
			$$ = ast_statement_block($1);
		}
	;

/*
	Syntactic sugars:

	if e {				->		if e {
	} else if e {		->		} else {
	}					->			if e {
						->			}
						->		}

	if e {				->		if e {
	} else if e {		->		} else {
	} else {			->			if e {
	}					->			} else {
						->			}
 						->		}
*/
else
	: /* empty */
		{
			$$ = NULL;
		}
	| TK_ELSE TK_IF expression block else
		{			
			Statement* statement = ($5)
				? ast_statement_if_else($3, $4, $5)
				: ast_statement_if($3, $4)
				;
			$$ = ast_block(ast_block_statement(statement));
		}
	| TK_ELSE block
		{
			$$ = $2;
		}
	;

// ==================================================
//
//	Variable
//
// ==================================================

variable
	: TK_LOWER_ID
		{
			$$ = ast_variable_id($1);
		}
	| primary_expression '[' expression ']'
		{
			$$ = ast_variable_indexed($1, $3);
		}
	;

// ==================================================
//
//	Expression
//
// ==================================================

expression
	: expression TK_OR expression
		{
			$$ = ast_expression_binary($2, TK_OR, $1, $3);
		}
	| expression TK_AND expression
		{
			$$ = ast_expression_binary($2, TK_AND, $1, $3);
		}
	| expression TK_EQUAL expression
		{
			$$ = ast_expression_binary($2, TK_EQUAL, $1, $3);
		}
	| expression TK_LEQUAL expression
		{
			$$ = ast_expression_binary($2, TK_LEQUAL, $1, $3);
		}
	| expression TK_GEQUAL expression
		{
			$$ = ast_expression_binary($2, TK_GEQUAL, $1, $3);
		}
	| expression '<' expression
		{
			$$ = ast_expression_binary($2, '<', $1, $3);
		}
	| expression '>' expression
		{
			$$ = ast_expression_binary($2, '>', $1, $3);
		}
	| expression '+' expression
		{
			$$ = ast_expression_binary($2, '+', $1, $3);
		}
	| expression '-' expression
		{
			$$ = ast_expression_binary($2, '-', $1, $3);
		}
	| expression '*' expression
		{
			$$ = ast_expression_binary($2, '*', $1, $3);
		}
	| expression '/' expression
		{
			$$ = ast_expression_binary($2, '/', $1, $3);
		}
	| '-' expression %prec TK_NOT
		{
			$$ = ast_expression_unary($1, '-', $2);
		}
	| TK_NOT expression
		{
			$$ = ast_expression_unary($1, TK_NOT, $2);
		}
	| primary_expression
		{
			$$ = $1;
		}
	;

primary_expression
	: literal
		{
			$$ = $1;
		}
	| variable
		{
			$$ = ast_expression_variable(0, $1); // TODO: Line
		}
	| function_call
		{
			$$ = ast_expression_function_call(0, $1); // TODO: Line
		}
	| '(' expression ')'
		{
			$$ = $2;
		}
	;

literal
	: TK_TRUE
		{
			$$ = ast_expression_literal_boolean($1, true);
		}
	| TK_FALSE
		{
			$$ = ast_expression_literal_boolean($1, false);
		}
	| TK_INTEGER
		{
			$$ = ast_expression_literal_integer(0, $1); // TODO: Line
		}
	| TK_FLOAT
		{
			$$ = ast_expression_literal_float(0, $1); // TODO: Line
		}
	| TK_STRING
		{
			$$ = ast_expression_literal_string(0, $1); // TODO: Line
		}
	;

// ==================================================
//
//	FunctionCall
//
// ==================================================

function_call
	: TK_LOWER_ID argument_list
		{
			$$ = ast_function_call_basic($1, $2);
		}
	| primary_expression '.' TK_LOWER_ID argument_list
		{
			$$ = ast_function_call_method($1, $3, $4);
		}
	| type argument_list
		{
			$$ = ast_function_call_constructor($1, $2);
		}
	;

argument_list
	: '(' ')'
		{
			$$ = NULL;
		}
	| '(' arguments ')'
		{
			$$ = $2;
		}
	;

arguments
	: expression
		{
			$$ = $1;
		}
	| arguments ',' expression
		{
			APPEND(Expression, $$, $1, $3);
		}
	;

// ==================================================
//
//	Class
//
// ==================================================

class_body
	: '{' class_content_list '}'
		{
			$$ = ast_body($2);
		}
	;

class_content_list
	: /* empty */
		{
			$$ = NULL;
		}
	| class_content_list class_content
		{
			APPEND(Body, $$, $1, $2);
		}
	;

class_content
	: variable_declaration ';'
		{
			WRAP_VARIABLE_DECLARATIONS($$, ast_body_declaration, $1, Body);
		}
	| variable_definition ';'
		{
			$$ = ast_body_definition($1);
		}
	| method_definition
		{
			$$ = ast_body_definition($1);
		}
	| constructor_definition
		{
			$$ = ast_body_definition($1);
		}
	;

method_definition
	: TK_PRIVATE function_definition
		{
			$$ = ast_definition_method($2, true);
		}
	| function_definition
		{
			$$ = ast_definition_method($1, false);
		}
	;

constructor_definition
	: TK_INITIALIZER parameter_list block
		{
			Declaration* declaration = ast_declaration_function(NULL, $2, NULL);
			$$ = ast_definition_constructor(declaration, $3);
		}
	;

// ==================================================
//
//	Auxiliary
//
// ==================================================

// Used by variable_declaration_base
lower_ids
	: TK_LOWER_ID
		{
			$$ = ast_declaration_variable(ast_variable_id($1));
		}
	| lower_ids ',' TK_LOWER_ID
		{
			Variable* variable = ast_variable_id($3);
			APPEND(Declaration, $$, $1, ast_declaration_variable(variable));
		}
	;

// Used by function declarations
parameter_list
	: /* empty */
		{
			$$ = NULL;
		}
	| '(' parameters ')'
		{
			$$ = $2;
		}
	;

parameters
	: parameter
		{
			$$ = $1;
		}
	| parameters ',' parameter
		{
			APPEND(Declaration, $$, $1, $3);
		}
	;

parameter
	: variable_declaration_base
		{
			for ($$ = $1; $1; $1 = $1->next) {
				$1->variable->value = false;
			}
		}
	;

// Used by block
block_variable_definition
	: variable_definition
		{
			$$ = $1;
		}
	/*
		Syntactic sugar:

		a := 1		->		value a: ? = 1
	*/
	| TK_LOWER_ID TK_DEFINE expression
		{
			Variable* var = ast_variable_id($1);
			var->value = true;
			$$ = ast_definition_variable(ast_declaration_variable(var), $3);
		}
	;

%%

static void yyerror(const char* err) {
	parser_error(0, (char*)err); // TODO: Line
}
