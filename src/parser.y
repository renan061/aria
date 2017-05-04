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
	TK_FUNCTION TK_DEFINE TK_WHILE TK_WAIT TK_IN TK_SIGNAL TK_BROADCAST
	TK_RETURN TK_IF TK_ELSE TK_FOR TK_SPAWN TK_TRUE TK_FALSE TK_MONITOR
	TK_PRIVATE TK_INITIALIZER

%token <ival> TK_INTEGER
%token <fval> TK_FLOAT
%token <strval> TK_STRING
%token <id> TK_LOWER_ID TK_UPPER_ID

// Nonterminals
%type <declaration>
	parameter_list parameters parameter variable_declaration lower_ids
%type <definition>
	function_definition method_definition constructor_definition monitor_definition
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
	: function_definition
		{
			$$ = ast_body_definition($1);
		}
	| monitor_definition
		{
			$$ = ast_body_definition($1);
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

parameter_list
	: '(' ')'
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
	: variable_declaration
		{
			$$ = $1;
		}
	;

variable_declaration
	: lower_ids ':' type
		{
			for ($$ = $1; $1; $1 = $1->next) {
				$1->variable.type = $3;
			}
		}
	;

lower_ids
	: TK_LOWER_ID
		{
			$$ = ast_declaration_variable($1, NULL);
		}
	| lower_ids ',' TK_LOWER_ID
		{
			APPEND(Declaration, $$, $1, ast_declaration_variable($3, NULL));
		}
	;

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
			$$ = ast_block_declaration($1);
		}
	| statement
		{
			$$ = ast_block_statement($1);
		}
	;

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
	| TK_LOWER_ID TK_DEFINE expression
		{
			$$ = ast_statement_definition($1, $3);
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
			$$ = ast_block_statement(statement);
		}
	| TK_ELSE block
		{
			$$ = $2;
		}
	;

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

expression
	: expression TK_OR expression
		{
			$$ = ast_expression_binary(TK_OR, $1, $3);
		}
	| expression TK_AND expression
		{
			$$ = ast_expression_binary(TK_AND, $1, $3);
		}
	| expression TK_EQUAL expression
		{
			$$ = ast_expression_binary(TK_EQUAL, $1, $3);
		}
	| expression TK_LEQUAL expression
		{
			$$ = ast_expression_binary(TK_LEQUAL, $1, $3);
		}
	| expression TK_GEQUAL expression
		{
			$$ = ast_expression_binary(TK_GEQUAL, $1, $3);
		}
	| expression '<' expression
		{
			$$ = ast_expression_binary('<', $1, $3);
		}
	| expression '>' expression
		{
			$$ = ast_expression_binary('>', $1, $3);
		}
	| expression '+' expression
		{
			$$ = ast_expression_binary('+', $1, $3);
		}
	| expression '-' expression
		{
			$$ = ast_expression_binary('-', $1, $3);
		}
	| expression '*' expression
		{
			$$ = ast_expression_binary('*', $1, $3);
		}
	| expression '/' expression
		{
			$$ = ast_expression_binary('/', $1, $3);
		}
	| '-' expression %prec TK_NOT
		{
			$$ = ast_expression_unary('-', $2);
		}
	| TK_NOT expression
		{
			$$ = ast_expression_unary(TK_NOT, $2);
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
			$$ = ast_expression_literal_boolean(true);
		}
	| TK_FALSE
		{
			$$ = ast_expression_literal_boolean(false);
		}
	| TK_INTEGER
		{
			$$ = ast_expression_literal_integer($1);
		}
	| TK_FLOAT
		{
			$$ = ast_expression_literal_float($1);
		}
	| TK_STRING
		{
			$$ = ast_expression_literal_string($1);
		}
	;

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

monitor_definition
	: TK_MONITOR TK_UPPER_ID class_body
		{
			$$ = ast_definition_monitor($2, $3);
		}
	;

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
			$$ = ast_body_declaration($1);
		}
	| constructor_definition
		{
			$$ = ast_body_definition($1);
		}
	| method_definition
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

%%

static void yyerror(const char* err) {
	parser_error(0, (char*)err); // TODO: Line
}
