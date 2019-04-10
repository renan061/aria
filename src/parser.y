/*
 * TODO:
 *  - Use yyltype for lines and position? Makes it slower...
 *  - Remove ';' occasionally
 *  - Type of monitor initializer return for function in ast
 */

%{
    #include <assert.h>
    #include <stdlib.h>

    #include "ast.h"
    #include "errs.h"
    #include "scanner.h"

    // Auxiliary macro to use with lists
    #define APPEND(type, assignable, list, elem) do { \
        if (!list) { \
            assignable = elem; \
        } else { \
            type* e; \
            for (e = assignable = list; e->next; e = e->next); \
            e->next = elem; \
        } \
    } while (0); \

    static void yyerror(const char* err);
%}

// semantic information
%union {
    // tokens
    int ival;
    struct {
        Line line;
        union {
            int ival;
            double fval;
            const char* strval;
        };
    } literal;

    // nonterminals
    Definition* definition;
    Id* id;
    Type* type;
    Block* block;
    Statement* statement;
    Capsa* capsa;
    Expression* expression;
    FunctionCall* function_call;
}

// tokens
%token <ival>
    '{' '}' '[' ']' '(' ')' '=' ';'
    TK_IMMUTABLE TK_VALUE TK_VARIABLE TK_FUNCTION TK_WHILE TK_WAIT
    TK_IN TK_SIGNAL TK_BROADCAST TK_RETURN TK_IF TK_ELSE TK_FOR TK_SPAWN TK_TRUE
    TK_FALSE TK_STRUCTURE TK_MONITOR TK_PRIVATE TK_INITIALIZER
    TK_DEF_ASG TK_ADD_ASG TK_SUB_ASG TK_MUL_ASG TK_DIV_ASG TK_INTERFACE
    TK_ACQUIRE TK_RELEASE

%token <literal> TK_INTEGER
%token <literal> TK_FLOAT
%token <literal> TK_STRING
%token <id> TK_LOWER_ID TK_UPPER_ID

// nonterminals
%type <definition> // file definitions
    file_definition file_definition_list
%type <definition> // capsa declarations
    capsa_declaration
    value_declaration variable_declaration
    parameter parameter_list0 parameter_list1
    lower_id_type lower_ids lower_ids_type
%type <definition> // capsa definitions
    capsa_definition block_capsa_definition
    value_definition variable_definition
    capsa_definition_assignment
%type <definition> // function declarations
    function_declaration
%type <definition> // function definitions
    function_definition method_definition constructor_definition
%type <definition> // interfaces
    interface_definition
    interface_element interface_element_list
%type <definition> // structures
    structure_definition
    structure_element structure_element_list
%type <definition> // monitors
    monitor_definition
    monitor_element_list monitor_element
%type <type>
    type
%type <block>
    block block_element_list block_element else
%type <statement>
    statement simple_statement compound_statement assignment
%type <capsa>
    capsa
%type <expression>
    expression_list0 expression_list1
    expression primary_expression literal
%type <function_call>
    function_call

// operator precedence and associativity
%left       <ival> TK_OR
%left       <ival> TK_AND
%nonassoc   <ival> TK_EQUAL TK_NEQUAL
%nonassoc   <ival> '<' '>' TK_LEQUAL TK_GEQUAL
%left       <ival> '+' '-'
%left       <ival> '*' '/'
%left       <ival> TK_NOT // precedence for unary minus
%left       <ival> TK_AS

// first nonterminal
%start program

%%

// ==================================================
//
//  Program
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
    : value_definition ';'
        {
            $1->capsa.capsa->global = true;
            $$ = $1;
        }
    | function_definition
        {
            $$ = $1;
        }
    | interface_definition
        {
            $$ = $1;
        }
    | structure_definition
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
//  Capsa - Declaration
//
// ==================================================

capsa_declaration
    : value_declaration
        {
            $$ = $1;
        }
    | variable_declaration
        {
            $$ = $1;
        }
    ;

value_declaration
    : TK_VALUE lower_ids_type
        {
            $2->capsa.capsa->value = true;
            $$ = $2;
        }

variable_declaration
    : TK_VARIABLE lower_ids_type
        {
            $2->capsa.capsa->value = false;
            $$ = $2;
        }
    ;

// ==================================================
//
//  Capsa - Definition
//
// ==================================================

capsa_definition
    : value_definition
        {
            $$ = $1;
        }
    | variable_definition
        {
            $$ = $1;
        }
    ;

value_definition
    : TK_VALUE capsa_definition_assignment
        {
            $2->capsa.capsa->value = true;
            $$ = $2;
        }
    ;

variable_definition
    : TK_VARIABLE capsa_definition_assignment
        {
            $2->capsa.capsa->value = false;
            $$ = $2;
        }
    ;

// auxiliary
capsa_definition_assignment
    : TK_LOWER_ID ':' type '=' expression
        {
            Capsa* capsa = ast_capsa_id($1);
            capsa->type = $3;
            $$ = ast_definition_capsa(capsa, $5);
        }
    | TK_LOWER_ID '=' expression
        {
            Capsa* capsa = ast_capsa_id($1);
            capsa->type = NULL;
            $$ = ast_definition_capsa(capsa, $3);
        }
    ;

// ==================================================
//
//  Function
//
// ==================================================

function_declaration
    : TK_FUNCTION TK_LOWER_ID parameter_list0 ':' type
        {
            $$ = ast_declaration_function($2, $3, $5);
        }
    | TK_FUNCTION TK_LOWER_ID parameter_list0
        {
            $$ = ast_declaration_function($2, $3, ast_type_void());
        }
    ;

function_definition
    : function_declaration block
        {
            $$ = ast_definition_function($1, $2);
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
    : TK_INITIALIZER parameter_list0 block
        {
            $$ = ast_definition_constructor($2, $3);
        }
    ;

// ==================================================
//
//  Type
//
// ==================================================

/* TODO: error handling for TK_IMMUTABLE TK_UPPER_ID */
type
    : TK_UPPER_ID
        {
            $$ = ast_type_id($1);
        }
    | '[' type ']'
        {
            $$ = ast_type_array($2);
        }
    | TK_IMMUTABLE '[' type ']'
        {
            $$ = ast_type_array($3);

            // TODO: Recursive immutability
            Type* type = $$;
            while (type) {
                switch (type->tag) {
                case TYPE_VOID:
                    type = NULL;
                    break;
                case TYPE_ID:
                    type = NULL;
                    break;
                case TYPE_ARRAY:
                    type->immutable = true;
                    type = type->array;
                    break;
                case TYPE_MONITOR:
                    type = NULL;
                    break;
                default:
                    assert(0); // TODO
                }
            }
        }
    ;

// ==================================================
//
//  Block
//
// ==================================================

block
    : '{' block_element_list '}'
        {
            $$ = ast_block($1, $2);
        }
    ;

block_element_list
    : /* empty */
        {
            $$ = NULL;
        }
    | block_element_list block_element
        {
            APPEND(Block, $$, $1, $2);
        }
    ;

block_element
    : variable_declaration ';'
        {
            $$ = ast_block_definition($1);
        }
    | block_capsa_definition ';'
        {
            $$ = ast_block_definition($1);
        }
    | statement
        {
            $$ = ast_block_statement($1);
        }
    ;

// auxiliary
block_capsa_definition
    : capsa_definition
        {
            $$ = $1;
        }
    // syntactic sugar: "a := 1" equals "value a: ? = 1"
    | TK_LOWER_ID TK_DEF_ASG expression
        {
            Capsa* capsa = ast_capsa_id($1);
            capsa->value = true;
            $$ = ast_definition_capsa(capsa, $3);
        }
    ;

// ==================================================
//
//  Statement
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
    : assignment
        {
            $$ = $1;
        }
    | function_call
        {
            $$ = ast_statement_function_call($1);
        }
    | TK_WAIT TK_FOR expression TK_IN expression
        {
            $$ = ast_statement_wait_for_in($1, $3, $5);
        }
    | TK_SIGNAL expression
        {
            $$ = ast_statement_signal($1, $2);
        }
    | TK_BROADCAST expression
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
    | TK_FOR variable_definition ';' expression ';' assignment block
        {
            $$ = ast_statement_for($1, $2, $4, $6, $7);
        }
    | TK_SPAWN block
        {
            $$ = ast_statement_spawn($1, $2);
        }
    | block
        {
            $$ = ast_statement_block($1);
        }
    ;

// auxiliary
assignment
    : capsa '=' expression
        {
            $$ = ast_statement_assignment($2, '=', $1, $3);
        }
    | capsa TK_ADD_ASG expression
        {
            $$ = ast_statement_assignment($2, TK_ADD_ASG, $1, $3);
        }
    | capsa TK_SUB_ASG expression
        {
            $$ = ast_statement_assignment($2, TK_SUB_ASG, $1, $3);
        }
    | capsa TK_MUL_ASG expression
        {
            $$ = ast_statement_assignment($2, TK_MUL_ASG, $1, $3);
        }
    | capsa TK_DIV_ASG expression
        {
            $$ = ast_statement_assignment($2, TK_DIV_ASG, $1, $3);
        }
    ;

/*
    Syntactic sugars:

    if e {              ->      if e {
    } else if e {       ->      } else {
    }                   ->          if e {
                        ->          }
                        ->      }

    if e {              ->      if e {
    } else if e {       ->      } else {
    } else {            ->          if e {
    }                   ->          } else {
                        ->          }
                        ->      }
*/

// auxiliary
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
//  Capsa
//
// ==================================================

capsa
    : TK_LOWER_ID
        {
            $$ = ast_capsa_id($1);
        }
    | primary_expression '.' TK_LOWER_ID
        {
            $$ = ast_capsa_attribute($1, $3);
        }
    | primary_expression '[' expression ']'
        {
            $$ = ast_capsa_indexed($2, $1, $3);
        }
    ;

// ==================================================
//
//  Expression
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
    | expression TK_NEQUAL expression
        {
            $$ = ast_expression_binary($2, TK_NEQUAL, $1, $3);
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
    | expression TK_AS type
        {
            $$ = ast_expression_cast($2, $1, $3);
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
    | capsa
        {
            $$ = ast_expression_capsa($1);
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
    | '[' expression_list1 ']'
        {
            $$ = ast_expression_literal_array($1, $2, false);
        }
    | TK_IMMUTABLE '[' expression_list1 ']'
        {
            $$ = ast_expression_literal_array($2, $3, true);
        }
    ;

// ==================================================
//
//  FunctionCall
//
// ==================================================

function_call
    : TK_LOWER_ID '(' expression_list0 ')'
        {
            $$ = ast_call($2, $1, $3);
        }
    | primary_expression '.' TK_LOWER_ID '(' expression_list0 ')'
        {
            $$ = ast_call_method($4, $1, $3, $5);
        }
    | type '(' expression_list0 ')'
        {
            $$ = ast_call_constructor($2, $1, $3);
        }
    ;

// ==================================================
//
//  Interface
//
// ==================================================

interface_definition
    : TK_INTERFACE TK_UPPER_ID '{' interface_element_list '}'
        {
            Type* type = ast_type_structure($2, TYPE_INTERFACE, $4, NULL);
            $$ = ast_definition_type(type);
        }
    ;

interface_element_list
    : /* empty */
        {
            $$ = NULL;
        }
    | interface_element_list interface_element
        {
            APPEND(Definition, $$, $1, $2);
        }
    ;

interface_element
    : function_declaration ';'
        {
            $$ = $1;
        }
    ;

// ==================================================
//
//  Structure
//
// ==================================================

structure_definition
    : TK_STRUCTURE TK_UPPER_ID '{' structure_element_list '}'
        {
            Type* type = ast_type_structure($2, TYPE_STRUCTURE, $4, NULL);
            $$ = ast_definition_type(type);
        }
    ;

structure_element_list
    : /* empty */
        {
            $$ = NULL;
        }
    | structure_element_list structure_element
        {
            APPEND(Definition, $$, $1, $2);
        }
    ;

structure_element
    : capsa_declaration ';'
        {
            $$ = $1;
        }
    | capsa_definition ';'
        {
            $$ = $1;
        }
    | constructor_definition
        {
            $$ = $1;
        }
    ;

// ==================================================
//
//  Monitor
//
// ==================================================

monitor_definition
    : TK_MONITOR TK_UPPER_ID ':' type '{' monitor_element_list '}'
        {
            Type* type = ast_type_structure($2, TYPE_MONITOR, $6, $4);
            $$ = ast_definition_type(type);
        }
    | TK_MONITOR TK_UPPER_ID '{' monitor_element_list '}'
        {
            Type* type = ast_type_structure($2, TYPE_MONITOR, $4, NULL);
            $$ = ast_definition_type(type);
        }
    ;

monitor_element_list
    : /* empty */
        {
            $$ = NULL;
        }
    | monitor_element_list monitor_element
        {
            APPEND(Definition, $$, $1, $2);
        }
    ;

monitor_element
    : variable_declaration ';'
        {
            $$ = $1;
        }
    | capsa_definition ';'
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

// ==================================================
//
//  Auxiliary
//
// ==================================================

// a comma separated list of zero or more expressions
expression_list0
    : /* empty */
        {
            $$ = NULL;
        }
    | expression_list1
        {
            $$ = $1;
        }
    ;

// a comma separated list of one or more expressions
expression_list1
    : expression
        {
            $$ = $1;
        }
    | expression_list1 ',' expression
        {
            Expression* e; // OBS: Can't use APPEND because it's a linked list
            for (e = $$ = $1; e->next; e = e->next);
            ($3->previous = e)->next = $3; // linking
        }
    ;

// used by capsa declarations and parameter_list1
lower_id_type
    : TK_LOWER_ID ':' type
        {
            Capsa* capsa = ast_capsa_id($1);
            capsa->type = $3;
            $$ = ast_definition_capsa(capsa, NULL);
        }
    ;

lower_ids
    : TK_LOWER_ID
        {
            $$ = ast_definition_capsa(ast_capsa_id($1), NULL);
        }
    | lower_ids ',' TK_LOWER_ID
        {
            APPEND(
                Definition, $$, $1, ast_definition_capsa(ast_capsa_id($3), NULL)
            );
        }
    ;

/*
    Syntactic sugar:

    a, b, c: Integer   ->   a: Integer
                       ->   b: Integer
                       ->   c: Integer
*/

lower_ids_type
    : lower_id_type
        {
            $$ = $1;
        }
    | lower_ids ',' lower_id_type
        {
            $$ = $1;
            Definition* last = NULL;
            for (; $1; last = $1, $1 = $1->next) {
                $1->capsa.capsa->type = $3->capsa.capsa->type;
            }
            last->next = $3; // there is always at least one lower_id
        }
    ; 

// used by function definitions
parameter_list0
    : /* empty */
        {
            $$ = NULL;
        }
    | '(' parameter_list1 ')'
        {
            $$ = $2;
        }
    ;

parameter_list1
    : parameter
        {
            $$ = $1;
        }
    | parameter_list1 ',' parameter
        {
            APPEND(Definition, $$, $1, $3);
        }
    ;

parameter
    : lower_ids_type
        {
            for ($$ = $1; $1; $1 = $1->next) {
                $1->capsa.capsa->value = true;
            }
        }
    ;

%%

static void yyerror(const char* err) {
    parser_error(0, (char*)err); // TODO: line
}
