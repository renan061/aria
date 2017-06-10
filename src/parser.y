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
	#define APPEND(type, assignable, list, elem); \
		if (!list) { \
			assignable = elem; \
		} else { \
			type* e; \
			for (e = assignable = list; e->next; e = e->next); \
			e->next = elem; \
		} \

	static void yyerror(const char* err);
%}

// Semantic information
%union {
	// Tokens
	int ival;
	struct {
		Line line;
		union {
			int ival;
			double fval;
			const char* strval;
		};
	} literal;

	// Nonterminals
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
	TK_IMMUTABLE TK_VALUE TK_VARIABLE TK_FUNCTION TK_DEFINE TK_WHILE TK_WAIT
	TK_IN TK_SIGNAL TK_BROADCAST TK_RETURN TK_IF TK_ELSE TK_FOR TK_SPAWN TK_TRUE
	TK_FALSE TK_MONITOR TK_PRIVATE TK_INITIALIZER

%token <literal> TK_INTEGER
%token <literal> TK_FLOAT
%token <literal> TK_STRING
%token <id> TK_LOWER_ID TK_UPPER_ID

// Nonterminals
%type <definition> // file definitions
	file_definition_list file_definition
%type <definition> // variable declarations
	variable_declaration_single variable_declaration_base  variable_declaration 
	parameter_list parameters parameter
	lower_ids
%type <definition> // variable definitions
	variable_definition block_variable_definition
	variable_definition_value variable_definition_variable
	variable_definition_base
%type <definition> // functions
	function_definition method_definition constructor_definition
%type <definition> // monitors
	monitor_definition
	monitor_body monitor_body_definition_list monitor_body_definition
%type <type>
	type
%type <block>
	block block_content_list block_content else
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
	: file_definition_list
		{
			ast_set($1);
		}
	;

file_definition_list
	: /* empty */
		{
			$$ = NULL;
		}
	| file_definition_list file_definition
		{
			APPEND(Definition, $$, $1, $2);
		}
	;

file_definition
	: variable_definition_value ';'
		{
			$1->variable.variable->global = true;
			$$ = $1;
		}
	| function_definition
		{
			$$ = $1;
		}
	| monitor_definition
		{
			$$ = $1;
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
			$$ = ast_definition_variable(variable, NULL);
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
			$$ = $1;
			Definition* last = NULL;
			for (; $1; last = $1, $1 = $1->next) {
				$1->variable.variable->type = $3->variable.variable->type;
			}
			last->next = $3; // there is always at least one lower_id
		}
	; 

variable_declaration
	: TK_VARIABLE variable_declaration_base
		{
			$2->variable.variable->value = false;
			$$ = $2;
		}
	;

// ==================================================
//
//	Definition
//
// ==================================================

variable_definition_base
	: TK_LOWER_ID ':' type '=' expression
		{
			Variable* variable = ast_variable_id($1);
			variable->type = $3;
			$$ = ast_definition_variable(variable, $5);
		}
	| TK_LOWER_ID '=' expression
		{
			$$ = ast_definition_variable(ast_variable_id($1), $3);
		}
	;

variable_definition_value
	: TK_VALUE variable_definition_base
		{
			$2->variable.variable->value = true;
			$$ = $2;
		}
	;

variable_definition_variable
	: TK_VARIABLE variable_definition_base
		{
			$2->variable.variable->value = false;
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
			$$ = ast_definition_function($2, $3, $5, $6);
		}
	| TK_FUNCTION TK_LOWER_ID parameter_list block
		{
			$$ = ast_definition_function($2, $3, ast_type_void(), $4);
		}
	;

monitor_definition
	: TK_MONITOR TK_UPPER_ID monitor_body
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
	| TK_IMMUTABLE TK_UPPER_ID
		{
			$$ = ast_type_id($2);
			$$->immutable = true;
		}
	| '[' type ']'
		{
			$$ = ast_type_array($2);
		}
	| TK_IMMUTABLE '[' type ']'
		{
			$$ = ast_type_array($3);
			$$->immutable = true;
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
			$$ = ast_block($1, $2);
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
			$$ = ast_block_definition($1);
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
			$$ = ast_statement_assignment($2, $1, $3);
		}
	| function_call
		{
			$$ = ast_statement_function_call($1);
		}
	| TK_WHILE expression TK_WAIT TK_IN variable
		{
			$$ = ast_statement_while_wait($1, $2, $5);
		}
	| TK_SIGNAL variable
		{
			$$ = ast_statement_signal($1, $2);
		}
	| TK_BROADCAST variable
		{
			$$ = ast_statement_broadcast($1, $2);
		}
	| TK_RETURN
		{
			$$ = ast_statement_return($1, NULL);
		}
	| TK_RETURN expression
		{
			$$ = ast_statement_return($1, $2);
		}
	;

compound_statement
	: TK_IF expression block else
		{
			$$ = ($4)
				? ast_statement_if_else($1, $2, $3, $4)
				: ast_statement_if($1, $2, $3)
				;
		}
	| TK_WHILE expression block
		{
			$$ = ast_statement_while($1, $2, $3);
		}
	/* | TK_FOR [?] ';' expression ';' [?] block */
	| TK_SPAWN block
		{
			$$ = ast_statement_spawn($1, $2);
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
				? ast_statement_if_else($2, $3, $4, $5)
				: ast_statement_if($2, $3, $4)
				;
			$$ = ast_block(statement->line, ast_block_statement(statement));
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
			$$ = ast_variable_indexed($2, $1, $3);
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
			$$ = ast_expression_variable($1);
		}
	| function_call
		{
			$$ = ast_expression_function_call($1);
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
			$$ = ast_expression_literal_integer($1.line, $1.ival);
		}
	| TK_FLOAT
		{
			$$ = ast_expression_literal_float($1.line, $1.fval);
		}
	| TK_STRING
		{
			$$ = ast_expression_literal_string($1.line, $1.strval);
		}
	;

// ==================================================
//
//	FunctionCall
//
// ==================================================

function_call
	: TK_LOWER_ID '(' argument_list ')'
		{
			$$ = ast_call($2, $1, $3);
		}
	| primary_expression '.' TK_LOWER_ID '(' argument_list ')'
		{
			$$ = ast_call_method($4, $1, $3, $5);
		}
	| type '(' argument_list ')'
		{
			$$ = ast_call_constructor($2, $1, $3);
		}
	;

// '(' and ')' are in function_call because of line numbers
argument_list
	: /* empty */
		{
			$$ = NULL;
		}
	| arguments
		{
			$$ = $1;
		}
	;

arguments
	: expression
		{
			$$ = $1;
		}
	| arguments ',' expression
		{
			Expression* e;
			for (e = $$ = $1; e->next; e = e->next);
			($3->previous = e)->next = $3; // linking
		}
	;

// ==================================================
//
//	Monitor
//
// ==================================================

monitor_body
	: '{' monitor_body_definition_list '}'
		{
			$$ = $2;
		}
	;

monitor_body_definition_list
	: /* empty */
		{
			$$ = NULL;
		}
	| monitor_body_definition_list monitor_body_definition
		{
			APPEND(Definition, $$, $1, $2);
		}
	;

monitor_body_definition
	: variable_declaration ';'
		{
			$$ = $1;
		}
	| variable_definition ';'
		{
			$$ = $1;
		}
	| method_definition
		{
			$$ = $1;
		}
	| constructor_definition
		{
			$$ = $1;
		}
	;

method_definition
	: TK_PRIVATE function_definition
		{
			$$ = ast_definition_method(true, $2);
		}
	| function_definition
		{
			$$ = ast_definition_method(false, $1);
		}
	;

constructor_definition
	: TK_INITIALIZER parameter_list block
		{
			$$ = ast_definition_constructor($2, $3);
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
			$$ = ast_definition_variable(ast_variable_id($1), NULL);
		}
	| lower_ids ',' TK_LOWER_ID
		{
			APPEND(Definition, $$, $1,
				ast_definition_variable(ast_variable_id($3), NULL));
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
			APPEND(Definition, $$, $1, $3);
		}
	;

parameter
	: variable_declaration_base
		{
			for ($$ = $1; $1; $1 = $1->next) {
				$1->variable.variable->value = true;
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
			Variable* variable = ast_variable_id($1);
			variable->value = true;
			$$ = ast_definition_variable(variable, $3);
		}
	;

%%

static void yyerror(const char* err) {
	parser_error(0, (char*)err); // TODO: Line
}
