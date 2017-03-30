/*
 * TODO:
 *	- Use yyltype for lines and position? Makes it slower...
 *	- Devo repassar o token NEWLINE pra controlar? Coisas como
 *		a, b: Integer c: String estão funcionando.
 */

%{
	#include "errs.h"
	#include "scanner.h"

	void yyerror(const char* err);
%}

%union {
	int ival;
	double dval;
	const char* strval; // TODO: Really constant?
}

%start program

// Operators
%left		<ival> TK_OR
%left		<ival> TK_AND
%nonassoc	<ival> TK_EQUAL TK_NEQUAL
%nonassoc	<ival> '<' '>' TK_LEQUAL TK_GEQUAL
%left		<ival> '+' '-'
%left		<ival> '*' '/'
%left		<ival> TK_NOT // precedence for unary minus

%token <ival>
	'{' '}' '[' ']' '(' ')' '=' ';'
	TK_FUNCTION TK_ASSIGN TK_WHILE TK_WAIT TK_IN TK_SIGNAL TK_BROADCAST
	TK_RETURN TK_IF TK_ELSE TK_FOR TK_SPAWN TK_TRUE TK_FALSE TK_MONITOR
	TK_PRIVATE TK_INITIALIZER

%token <ival> TK_INTEGER
%token <dval> TK_FLOAT
%token <strvalue> TK_STRING TK_LOWER_ID TK_UPPER_ID

%%

program
	: definitions
	;

definitions
	: /* empty */
	| definitions definition
	;

definition
	: function_definition
	| monitor_definition
	;

function_definition
	: TK_FUNCTION TK_LOWER_ID parameters ':' type block
	| TK_FUNCTION TK_LOWER_ID parameters block
	;

parameters
	: '(' ')'
	| '(' parameter_list ')'
	;

parameter_list
	: parameter
	| parameter_list ',' parameter
	;

parameter
	: variable_declaration

variable_declaration
	: lower_id_list ':' type
	;

lower_id_list
	: TK_LOWER_ID
	| lower_id_list ',' TK_LOWER_ID
	;

type
	: TK_UPPER_ID
	| '[' type ']'
	;

block
	: '{' block_content_list '}'
	;

block_content_list
	: /* empty */
	| block_content_list block_content
	;

/* TODO: Newline after variable_declaration? */
block_content
	: variable_declaration
	| statement
	;

/* TODO: Newline after simple_statement? */
/* TODO: Removing ';' causes a _lot_ of conflicts */
statement
	: simple_statement ';'
	| compound_statement
	;

simple_statement
	: variable '=' expression
	| TK_LOWER_ID TK_ASSIGN expression
	| function_call
	| TK_WHILE expression TK_WAIT TK_IN variable
	| TK_SIGNAL variable
	| TK_BROADCAST variable
	| TK_RETURN
	| TK_RETURN expression
	;

compound_statement
	: TK_IF expression block
	| TK_IF expression block TK_ELSE block
	| TK_WHILE expression block
	/* | TK_FOR [?] ';' expression ';' [?] block */
	| TK_SPAWN block
	| block
	;

variable
	: TK_LOWER_ID
	| primary_expression '[' expression ']'
	;

expression
	: expression TK_OR expression
	| expression TK_AND expression
	| expression TK_EQUAL expression
	| expression TK_LEQUAL expression
	| expression TK_GEQUAL expression
	| expression '<' expression
	| expression '>' expression
	| expression '+' expression
	| expression '-' expression
	| expression '*' expression
	| expression '/' expression
	| '-' expression %prec TK_NOT
	| TK_NOT expression
	| primary_expression
	;

primary_expression
	: literal
	| variable
	| function_call
	| '(' expression ')'
	;

literal
	: TK_TRUE
	| TK_FALSE
	| TK_INTEGER
	| TK_FLOAT
	| TK_STRING
	;

function_call
	: TK_LOWER_ID arguments
	| type arguments
	| primary_expression '.' TK_LOWER_ID arguments
	;

arguments
	: '(' ')'
	| '(' expression_list ')'
	;

expression_list
	: expression
	| expression_list ',' expression
	;

/* TODO */
monitor_definition
	: TK_MONITOR
	;

%%

void yyerror(const char* err) {
	parser_error(0, (char*)err); // TODO: Line
}
