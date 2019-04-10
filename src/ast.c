#include <assert.h>
#include <stdio.h> // TODO: Remove
#include <string.h>

#include "alloc.h"
#include "ast.h"
#include "errs.h"
#include "parser.h" // for the tokens
#include "scanner.h" // for native_types

// ==================================================
//
//  AST
//
// ==================================================

AST* ast = NULL;

void ast_set(Definition* definitions) {
    MALLOC(ast, AST);
    ast->definitions = definitions;
}

// ==================================================
//
//  Definition
//
// ==================================================

Definition* ast_declaration_function(Id* id, Definition* parameters, Type* t) {
    Definition* d;
    MALLOC(d, Definition);
    d->tag = DECLARATION_FUNCTION;
    d->next = NULL;
    d->llvm_value = NULL;
    d->function.qualifiers = 0; // bitmap zero
    d->function.id = id;
    d->function.parameters = parameters;
    d->function.type = t;
    d->function.block = NULL;
    d->function.vmt_index = -1;
    return d;
}

Definition* ast_definition_capsa(Capsa* capsa, Expression* expression) {
    Definition* definition;
    MALLOC(definition, Definition);
    definition->tag = DEFINITION_CAPSA;
    definition->next = NULL;
    definition->llvm_value = NULL;
    definition->capsa.capsa = capsa;
    definition->capsa.expression = expression;
    return definition;
}

Definition* ast_definition_function(Definition* d, Block* block) {
    assert(d->tag == DECLARATION_FUNCTION && block->tag == BLOCK);
    d->tag = DEFINITION_FUNCTION;
    d->function.block = block;
    return d;
}

Definition* ast_definition_constructor(Definition* parameters, Block* block) {
    Definition* definition;
    MALLOC(definition, Definition);
    assert(block->tag == BLOCK);
    definition->tag = DEFINITION_CONSTRUCTOR;
    definition->next = NULL;
    definition->llvm_value = NULL;
    definition->function.qualifiers = 0; // bitmap zero
    definition->function.id = NULL;
    definition->function.parameters = parameters;
    definition->function.type = NULL;
    definition->function.block = block;
    return definition;
}

Definition* ast_definition_type(Type* type) {
    assert(
        type->tag == TYPE_ID ||
        type->tag == TYPE_INTERFACE ||
        type->tag == TYPE_STRUCTURE ||
        type->tag == TYPE_MONITOR
    );
    Definition* definition;
    MALLOC(definition, Definition);
    definition->tag = DEFINITION_TYPE;
    definition->next = NULL;
    definition->llvm_value = NULL;
    definition->type = type;
    return definition;
}

// ==================================================
//
//  Id
//
// ==================================================

Id* ast_id(Line line, const char* name) {
    Id* id;
    MALLOC(id, Id);
    id->line = line;
    id->name = name;
    return id;
}

// ==================================================
//
//  Type
//
// ==================================================

// TODO: Test static
#define NATIVE_TYPE(v, i) \
    static Type* v = NULL; \
    if (!v) { \
        MALLOC(v, Type); \
        v->tag = TYPE_ID; \
        v->primitive = true; \
        v->immutable = true; \
        v->llvm_type = NULL; \
        v->id = ast_id(-1, scanner_native[i]); \
    } \
    return v; \

Type* ast_type_void(void) {
    static Type* type_void = NULL;
    if (!type_void) {
        MALLOC(type_void, Type);
        type_void->tag = TYPE_VOID;
        type_void->primitive = true;
        type_void->immutable = true;
        type_void->llvm_type = NULL;
        type_void->id = NULL; // just in case
    }
    return type_void;
}

Type* ast_type_boolean(void) {
    NATIVE_TYPE(type_boolean, SCANNER_NATIVE_BOOLEAN);
}

Type* ast_type_integer(void) {
    NATIVE_TYPE(type_integer, SCANNER_NATIVE_INTEGER);
}

Type* ast_type_float(void) {
    NATIVE_TYPE(type_float, SCANNER_NATIVE_FLOAT);
}

Type* ast_type_string(void) {
    NATIVE_TYPE(type_string, SCANNER_NATIVE_STRING);
}

Type* ast_type_condition_queue(void) {
    // TODO: Refactor
    static Type* type_condition_queue = NULL;
    if (!type_condition_queue) {
        MALLOC(type_condition_queue, Type);
        type_condition_queue->tag = TYPE_ID;
        type_condition_queue->primitive = true;
        type_condition_queue->immutable = false;
        type_condition_queue->llvm_type = NULL;
        type_condition_queue->id = ast_id(
            -1, scanner_native[SCANNER_NATIVE_CONDITION_QUEUE]
        );
    }
    return type_condition_queue;
}

static Type* checknative(Id* id) {
    #define CHECK_TYPE(i, v) \
        if (scanner_native[i] == id->name) { \
            return free(id), v; \
        } \

    CHECK_TYPE(SCANNER_NATIVE_BOOLEAN, ast_type_boolean());
    CHECK_TYPE(SCANNER_NATIVE_INTEGER, ast_type_integer());
    CHECK_TYPE(SCANNER_NATIVE_FLOAT, ast_type_float());
    CHECK_TYPE(SCANNER_NATIVE_STRING, ast_type_string());
    CHECK_TYPE(SCANNER_NATIVE_CONDITION_QUEUE, ast_type_condition_queue());
    return NULL;
}

Type* ast_type_id(Id* id) {
    Type* type = checknative(id);
    if (type) {
        return type;
    }
    MALLOC(type, Type);
    type->tag = TYPE_ID;
    type->primitive = false;
    type->immutable = false;
    type->llvm_type = NULL;
    type->id = id;
    return type;
}

Type* ast_type_array(Type* type) {
    Type* arrayType;
    MALLOC(arrayType, Type);
    arrayType->tag = TYPE_ARRAY;
    arrayType->primitive = false;
    arrayType->immutable = false;
    arrayType->llvm_type = NULL;
    arrayType->array = type;
    return arrayType;
}

Type* ast_type_structure(Id* id, TypeTag ttag, Definition* defs, Type* itype) {
    Type* type;
    MALLOC(type, Type);
    type->tag = ttag;
    type->primitive = false;
    type->immutable = false;
    type->llvm_type = NULL;
    type->structure.id = id;
    type->structure.interface = itype;
    type->structure.definitions = defs;
    type->structure.attributes = NULL;
    type->structure.attributes_size = -1;
    type->structure.methods = NULL;
    type->structure.methods_size = -1;
    type->structure.constructor = NULL;
    return type;
}

// ==================================================
//
//  Block
//
// ==================================================

Block* ast_block(Line ln, Block* block) {
    Block* base;
    MALLOC(base, Block);
    base->tag = BLOCK;
    base->line = ln;
    base->next = block;
    return base;
}

Block* ast_block_definition(Definition* definition) {
    Block* block;
    MALLOC(block, Block);
    block->tag = BLOCK_DEFINITION;
    block->line = 0; // should not be accessed
    block->next = NULL;
    block->definition = definition;
    return block;
}

Block* ast_block_statement(Statement* statement) {
    Block* block;
    MALLOC(block, Block);
    block->tag = BLOCK_STATEMENT;
    block->line = statement->line;
    block->next = NULL;
    block->statement = statement;
    return block;
}

// ==================================================
//
//  Statement
//
// ==================================================

Statement* ast_statement_assignment(Line ln, Token t, Capsa* v, Expression* e) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_ASSIGNMENT;
    statement->line = ln;

    switch (t) {
    case '=':
        break;
    case TK_ADD_ASG:
        /* fallthrough */
    case TK_SUB_ASG:
        /* fallthrough */
    case TK_MUL_ASG:
        /* fallthrough */
    case TK_DIV_ASG: {
        Token op;
        switch (t) {
        case TK_ADD_ASG: op = '+'; break;
        case TK_SUB_ASG: op = '-'; break;
        case TK_MUL_ASG: op = '*'; break;
        case TK_DIV_ASG: op = '/'; break;
        default: UNREACHABLE;
        }

        Capsa* copy = v;  // TODO: copy function for Capsa
        Expression* expression_capsa = ast_expression_capsa(copy);
        e = ast_expression_binary(ln, op, expression_capsa, e);
        break;
    }
    default:
        UNREACHABLE;
    }

    statement->assignment.capsa = v;
    statement->assignment.expression = e;
    return statement;
}

Statement* ast_statement_function_call(FunctionCall* function_call) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_FUNCTION_CALL;
    statement->line = function_call->line;
    statement->function_call = function_call;
    return statement;
}

Statement* ast_statement_wait_for_in(Line ln, Expression* c, Expression* q) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_WAIT_FOR_IN;
    statement->line = ln;
    statement->wait_for_in.condition = c;
    statement->wait_for_in.queue = q;
    return statement;
}

Statement* ast_statement_signal(Line ln, Expression* expression) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_SIGNAL;
    statement->line = ln;
    statement->signal = expression;
    return statement;
}

Statement* ast_statement_broadcast(Line ln, Expression* expression) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_BROADCAST;
    statement->line = ln;
    statement->broadcast = expression;
    return statement;
}

Statement* ast_statement_return(Line ln, Expression* expression) {
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_RETURN;
    statement->line = ln;
    statement->return_ = expression;
    return statement;
}

Statement* ast_statement_if(Line ln, Expression* expression, Block* block) {
    assert(block->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_IF;
    statement->line = ln;
    statement->if_.expression = expression;
    statement->if_.block = block;
    return statement;
}

// c -> condition, i -> if_block, e -> else_block
Statement* ast_statement_if_else(Line ln, Expression* c, Block* i, Block* e) {
    assert(i->tag == BLOCK && e->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_IF_ELSE;
    statement->line = ln;
    statement->if_else.expression = c;
    statement->if_else.if_block = i;
    statement->if_else.else_block = e;
    return statement;
}

Statement* ast_statement_while(Line ln, Expression* expression, Block* block) {
    assert(block->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_WHILE;
    statement->line = ln;
    statement->while_.expression = expression;
    statement->while_.block = block;
    return statement;
}

Statement* ast_statement_for(Line ln,
    Definition* initialization,
    Expression* condition,
    Statement* increment,
    Block* block) {

    assert(block->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_FOR;
    statement->line = ln;
    statement->for_.initialization = initialization;
    statement->for_.condition = condition;
    statement->for_.increment = increment;
    statement->for_.block = block;
    return statement;
}

Statement* ast_statement_spawn(Line ln, Block* block) {
    assert(block->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_SPAWN;
    statement->line = ln;
    statement->spawn = ast_call(ln, NULL, NULL);
    statement->spawn->function_definition = ast_definition_function(
        ast_declaration_function(NULL, NULL, ast_type_void()), block
    );
    return statement;
}

Statement* ast_statement_block(Block* block) {
    assert(block->tag == BLOCK);
    Statement* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_BLOCK;
    statement->line = 0; // TODO
    statement->block = block;
    return statement;
}

// ==================================================
//
//  Capsa
//
// ==================================================

static Capsa* capsadefaults() {
    Capsa* capsa;
    MALLOC(capsa, Capsa);
    capsa->type = NULL;
    capsa->global = false;
    capsa->value = false;
    capsa->llvm_value = NULL;
    capsa->llvm_structure_index = -1;
    return capsa;
}

Capsa* ast_capsa_id(Id* id) {
    Capsa* capsa = capsadefaults();
    capsa->tag = CAPSA_ID;
    capsa->line = id->line;
    capsa->id = id;
    return capsa;
}

Capsa* ast_capsa_attribute(Expression* expression, Id* id) {
    Capsa* capsa = capsadefaults();
    capsa->tag = CAPSA_ATTRIBUTE;
    capsa->line = id->line;
    capsa->attribute.structure = expression;
    capsa->attribute.field = id;
    return capsa;
}

Capsa* ast_capsa_indexed(Line ln, Expression* array, Expression* index) {
    Capsa* capsa = capsadefaults();
    capsa->tag = CAPSA_INDEXED;
    capsa->line = ln;
    capsa->indexed.array = array;
    capsa->indexed.index = index;
    return capsa;
}

// ==================================================
//
//  Expression
//
// ==================================================

Expression* ast_expression_literal_boolean(Line ln, bool literal_boolean) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_LITERAL_BOOLEAN;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->literal.immutable = true;
    expression->literal.boolean = literal_boolean;
    return expression;
}

Expression* ast_expression_literal_integer(Line ln, int literal_integer) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_LITERAL_INTEGER;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->literal.immutable = true;
    expression->literal.integer = literal_integer;
    return expression;
}

Expression* ast_expression_literal_float(Line ln, double literal_float) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_LITERAL_FLOAT;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->literal.immutable = true;
    expression->literal.float_ = literal_float;
    return expression;
}

Expression* ast_expression_literal_string(Line ln, const char* literal_string) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_LITERAL_STRING;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->literal.immutable = true;
    expression->literal.string = literal_string;
    return expression;
}

Expression* ast_expression_literal_array(Line ln, Expression* elements,
    bool immutable) {

    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_LITERAL_ARRAY;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->literal.immutable = immutable;
    expression->literal.array = elements;
    return expression;
}

Expression* ast_expression_capsa(Capsa* capsa) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_CAPSA;
    expression->line = capsa->line;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->capsa = capsa;
    return expression;
}

Expression* ast_expression_function_call(FunctionCall* function_call) {
    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_FUNCTION_CALL;
    expression->line = function_call->line;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->function_call = function_call;
    return expression;
}

Expression* ast_expression_unary(Line ln, Token token, Expression* expression) {
    Expression* unaryExpression;
    MALLOC(unaryExpression, Expression);
    unaryExpression->tag = EXPRESSION_UNARY;
    unaryExpression->line = ln;
    unaryExpression->previous = unaryExpression->next = NULL;
    unaryExpression->type = NULL;
    unaryExpression->llvm_value = NULL;
    unaryExpression->unary.token = token;
    unaryExpression->unary.expression = expression;
    return unaryExpression;
}

Expression* ast_expression_binary(Line ln, Token t, Expression* l,
    Expression* r) {

    Expression* expression;
    MALLOC(expression, Expression);
    expression->tag = EXPRESSION_BINARY;
    expression->line = ln;
    expression->previous = expression->next = NULL;
    expression->type = NULL;
    expression->llvm_value = NULL;
    expression->binary.token = t;
    expression->binary.left_expression = l;
    expression->binary.right_expression = r;
    return expression;
}

Expression* ast_expression_cast(Line ln, Expression* expression, Type* type) {
    // TODO: Remove when 'as' gets in the language
    assert(expression->type != type);

    Expression* castExpression;
    MALLOC(castExpression, Expression);
    castExpression->tag = EXPRESSION_CAST;
    castExpression->line = ln;
    castExpression->type = type;
    castExpression->llvm_value = NULL;
    castExpression->cast = expression;

    // rearranging the list (only for arguments)
    castExpression->previous = expression->previous;
    expression->previous = NULL;
    castExpression->next = expression->next;
    expression->next = NULL;
    if (castExpression->previous) {
        castExpression->previous->next = castExpression;
    }
    if (castExpression->next) {
        castExpression->next->previous = castExpression;
    }

    return castExpression;
}

// ==================================================
//
//  Function Call
//
// ==================================================

FunctionCall* ast_call(Line ln, Id* id, Expression* arguments) {
    FunctionCall* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_BASIC;
    function_call->line = ln;
    function_call->type = NULL;
    function_call->instance = NULL;
    function_call->id = id;
    function_call->arguments = arguments;
    function_call->argument_count = -1;
    function_call->function_definition = NULL;
    return function_call;
}

FunctionCall* ast_call_method(Line ln, Expression* i, Id* id, Expression* a) {
    FunctionCall* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_METHOD;
    function_call->line = ln;
    function_call->type = NULL;
    function_call->instance = i;
    function_call->id = id;
    function_call->arguments = a;
    function_call->argument_count = -1;
    function_call->function_definition = NULL;
    return function_call;
}

FunctionCall* ast_call_constructor(Line ln, Type* type, Expression* arguments) {
    FunctionCall* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
    function_call->line = ln;
    function_call->type = type;
    function_call->instance = NULL;
    function_call->id = NULL;
    function_call->arguments = arguments;
    function_call->argument_count = -1;
    function_call->function_definition = NULL;
    return function_call;
}
