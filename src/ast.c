#include <assert.h>
#include <stdio.h> // TODO: Remove
#include <string.h>

#include "alloc.h"
#include "ast.h"
#include "macros.h"
#include "parser.h" // for the tokens
#include "scanner.h" // for native_types

// ==================================================
//
//  AST
//
// ==================================================

AST* ast = NULL;

void ast_set(Def* definitions) {
    MALLOC(ast, AST);
    ast->definitions = definitions;
}

// ==================================================
//
//  Definition
//
// ==================================================

Def* ast_declaration_function(Id* id, Def* parameters, Type* t) {
    Def* d;
    MALLOC(d, Definition);
    d->tag = DECLARATION_FUNCTION;
    d->next = NULL;
    d->V = NULL;
    d->function.qualifiers = 0; // bitmap zero
    d->function.id = id;
    d->function.parameters = parameters;
    d->function.type = t;
    d->function.block = NULL;
    d->function.vmti = -1;
    d->function.pair = NULL;
    return d;
}

Def* ast_definition_capsa(Capsa* capsa, Exp* expression) {
    Def* definition;
    MALLOC(definition, Definition);
    definition->tag = DEFINITION_CAPSA;
    definition->next = NULL;
    definition->V = NULL;
    definition->capsa.capsa = capsa;
    definition->capsa.expression = expression;
    return definition;
}

Def* ast_definition_function(Def* d, Block* block) {
    assert(d->tag == DECLARATION_FUNCTION && block->tag == BLOCK);
    d->tag = DEFINITION_FUNCTION;
    d->function.block = block;
    return d;
}

Def* ast_definition_constructor(Def* parameters, Block* block) {
    Def* definition;
    MALLOC(definition, Definition);
    assert(block->tag == BLOCK);
    definition->tag = DEFINITION_CONSTRUCTOR;
    definition->next = NULL;
    definition->V = NULL;
    definition->function.qualifiers = 0; // bitmap zero
    definition->function.id = NULL;
    definition->function.parameters = parameters;
    definition->function.type = NULL;
    definition->function.block = block;
    return definition;
}

Def* ast_definition_type(Type* type) {
    assert(
        type->tag == TYPE_ID ||
        type->tag == TYPE_INTERFACE ||
        type->tag == TYPE_STRUCTURE ||
        type->tag == TYPE_MONITOR
    );
    Def* definition;
    MALLOC(definition, Definition);
    definition->tag = DEFINITION_TYPE;
    definition->next = NULL;
    definition->V = NULL;
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

// TODO: test static
#define NATIVE_TYPE(v, i) \
    static Type* v = NULL; \
    if (!v) { \
        MALLOC(v, Type); \
        v->tag = TYPE_ID; \
        v->primitive = true; \
        v->immutable = true; \
        v->T = NULL; \
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
        type_void->T = NULL;
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
    // TODO: refactor
    static Type* type_condition_queue = NULL;
    if (!type_condition_queue) {
        MALLOC(type_condition_queue, Type);
        type_condition_queue->tag = TYPE_ID;
        type_condition_queue->primitive = true;
        type_condition_queue->immutable = false;
        type_condition_queue->T = NULL;
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
    type->T = NULL;
    type->id = id;
    return type;
}

Type* ast_type_unlocked(Type* type) {
    Type* t;
    MALLOC(t, Type);
    t->tag = TYPE_UNLOCKED;
    t->primitive = false;
    t->immutable = false;
    t->T = NULL;
    t->unlocked = type;
    return t;
}

Type* ast_type_array(Type* type) {
    Type* arrayType;
    MALLOC(arrayType, Type);
    arrayType->tag = TYPE_ARRAY;
    arrayType->primitive = false;
    arrayType->immutable = false;
    arrayType->T = NULL;
    arrayType->array = type;
    return arrayType;
}

Type* ast_type_structure(Id* id, TypeTag ttag, Def* defs, Type* itype) {
    Type* type;
    MALLOC(type, Type);
    type->tag = ttag;
    type->primitive = false;
    type->immutable = false;
    type->T = NULL;
    type->structure.id = id;
    type->structure.interface = itype;
    type->structure.definitions = defs;
    type->structure.attributes = NULL;
    type->structure.attributes_size = -1;
    type->structure.methods = NULL;
    type->structure.methods_size = -1;
    type->structure.constructor = NULL;
    type->structure.gR = NULL;
    type->structure.gL = NULL;
    type->structure.gP = NULL;
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

Block* ast_block_definition(Def* definition) {
    Block* block;
    MALLOC(block, Block);
    block->tag = BLOCK_DEFINITION;
    block->line = 0; // should not be accessed
    block->next = NULL;
    block->definition = definition;
    return block;
}

Block* ast_block_statement(Stmt* statement) {
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

static Stmt* stmtinit(StatementTag tag, Line ln) {
    Stmt* stmt;
    MALLOC(stmt, Stmt);
    stmt->tag = tag;
    stmt->line = ln;
    return stmt;
}

Stmt* ast_statement_assignment(Line ln, Token t, Capsa* v, Exp* e) {
    Stmt* statement;
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
        Exp* expression_capsa = ast_expression_capsa(copy);
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

Stmt* ast_statement_function_call(FC* function_call) {
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_FUNCTION_CALL;
    statement->line = function_call->line;
    statement->function_call = function_call;
    return statement;
}

Stmt* ast_statement_wait_for_in(Line ln, Exp* c, Exp* q) {
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_WAIT_FOR_IN;
    statement->line = ln;
    statement->wait_for_in.condition = c;
    statement->wait_for_in.queue = q;
    return statement;
}

Stmt* ast_statement_signal(Line ln, Exp* expression) {
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_SIGNAL;
    statement->line = ln;
    statement->signal = expression;
    return statement;
}

Stmt* ast_statement_broadcast(Line ln, Exp* expression) {
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_BROADCAST;
    statement->line = ln;
    statement->broadcast = expression;
    return statement;
}

Stmt* ast_statement_return(Line ln, Exp* expression) {
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_RETURN;
    statement->line = ln;
    statement->return_ = expression;
    return statement;
}

Stmt* ast_statement_if(Line ln, Exp* expression, Block* block) {
    assert(block->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_IF;
    statement->line = ln;
    statement->if_.expression = expression;
    statement->if_.block = block;
    return statement;
}

// c -> condition, i -> if_block, e -> else_block
Stmt* ast_statement_if_else(Line ln, Exp* c, Block* i, Block* e) {
    assert(i->tag == BLOCK && e->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_IF_ELSE;
    statement->line = ln;
    statement->if_else.expression = c;
    statement->if_else.if_block = i;
    statement->if_else.else_block = e;
    return statement;
}

Stmt* ast_statement_while(Line ln, Exp* expression, Block* block) {
    assert(block->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_WHILE;
    statement->line = ln;
    statement->while_.expression = expression;
    statement->while_.block = block;
    return statement;
}

Stmt* ast_stmt_numeric_for(Line ln, Id* id, Exp* range, Block* block) {
    assert(block->tag == BLOCK);
    Stmt* stmt = stmtinit(STATEMENT_NUMERIC_FOR, ln);
    Capsa* capsa = ast_capsa_id(id);
    capsa->value = true;
    stmt->numeric_for.v = ast_definition_capsa(capsa, NULL);
    stmt->numeric_for.range = range;
    stmt->numeric_for.block = block;
    return stmt;
}

Stmt* ast_statement_for(Line ln,
    Def* initialization,
    Exp* condition,
    Stmt* increment,
    Block* block) {

    assert(block->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_FOR;
    statement->line = ln;
    statement->for_.initialization = initialization;
    statement->for_.condition = condition;
    statement->for_.increment = increment;
    statement->for_.block = block;
    return statement;
}

Stmt* ast_statement_spawn(Line ln, Block* block) {
    assert(block->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_SPAWN;
    statement->line = ln;
    statement->spawn = ast_call(ln, NULL, NULL);
    statement->spawn->fn = ast_definition_function(
        ast_declaration_function(NULL, NULL, ast_type_void()), block
    );
    return statement;
}

Stmt* ast_statement_acquire_value(Line ln, Def* v, Block* b) {
    assert(b->tag == BLOCK);
    Stmt* statement;
    MALLOC(statement, Statement);
    statement->tag = STATEMENT_ACQUIRE_VALUE;
    statement->line = ln;
    statement->acquire_value.value = v;
    statement->acquire_value.block = b;
    return statement;
}

Stmt* ast_statement_block(Block* block) {
    assert(block->tag == BLOCK);
    Stmt* statement;
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

static Capsa* capsa_init() {
    Capsa* capsa;
    MALLOC(capsa, Capsa);
    capsa->type = NULL;
    capsa->global = false;
    capsa->value = false;
    capsa->V = NULL;
    capsa->llvm_structure_index = -1;
    return capsa;
}

Capsa* ast_capsa_id(Id* id) {
    Capsa* capsa = capsa_init();
    capsa->tag = CAPSA_ID;
    capsa->line = id->line;
    capsa->id = id;
    return capsa;
}

Capsa* ast_capsa_attribute(Exp* expression, Id* id) {
    Capsa* capsa = capsa_init();
    capsa->tag = CAPSA_ATTRIBUTE;
    capsa->line = id->line;
    capsa->attribute.structure = expression;
    capsa->attribute.field = id;
    return capsa;
}

Capsa* ast_capsa_indexed(Line ln, Exp* array, Exp* index) {
    Capsa* capsa = capsa_init();
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

static Exp* exp_init(ExpressionTag tag, Line ln) {
    Exp* exp;
    MALLOC(exp, Exp);
    exp->tag = tag;
    exp->line = ln;
    exp->previous = exp->next = NULL;
    exp->type = NULL;
    exp->V = NULL;
    return exp;
}

Exp* ast_expression_literal_boolean(Line ln, bool boolean) {
    Exp* exp = exp_init(EXPRESSION_LITERAL_BOOLEAN, ln);
    exp->literal.immutable = true;
    exp->literal.boolean = boolean;
    return exp;
}

Exp* ast_expression_literal_integer(Line ln, int integer) {
    Exp* exp = exp_init(EXPRESSION_LITERAL_INTEGER, ln);
    exp->literal.immutable = true;
    exp->literal.integer = integer;
    return exp;
}

Exp* ast_expression_literal_float(Line ln, double float_) {
    Exp* exp = exp_init(EXPRESSION_LITERAL_FLOAT, ln);
    exp->literal.immutable = true;
    exp->literal.float_ = float_;
    return exp;
}

Exp* ast_expression_literal_string(Line ln, const char* string) {
    Exp* exp = exp_init(EXPRESSION_LITERAL_STRING, ln);
    exp->literal.immutable = true;
    exp->literal.string = string;
    return exp;
}

Exp* ast_expression_literal_array(Line ln, Exp* array, bool immutable) {
    Exp* exp = exp_init(EXPRESSION_LITERAL_ARRAY, ln);
    exp->literal.immutable = immutable;
    exp->literal.array = array;
    return exp;
}

Exp* ast_expression_capsa(Capsa* capsa) {
    Exp* exp = exp_init(EXPRESSION_CAPSA, capsa->line);
    exp->capsa = capsa;
    return exp;
}

Exp* ast_expression_function_call(FC* fc) {
    Exp* exp = exp_init(EXPRESSION_FUNCTION_CALL, fc->line);
    exp->function_call = fc;
    return exp;
}

Exp* ast_expression_comprehension(Line ln, Exp* e, Id* i, Exp* l, Exp* u) {
    Exp* exp = exp_init(EXPRESSION_LIST_COMPREHENSION, ln);
    exp->comprehension.e = e;
    Capsa* capsa = ast_capsa_id(i);
    capsa->type = ast_type_integer();
    capsa->value = true;
    exp->comprehension.i = ast_definition_capsa(capsa, l);
    exp->comprehension.lower = l;
    exp->comprehension.upper = u;
    exp->comprehension.immutable = false;
    return exp;
}

Exp* ast_expression_range(Line ln, Token op, Exp* f, Exp* s, Exp* l) {
    Exp* exp = exp_init(EXPRESSION_RANGE, ln);
    exp->range.op = op;
    exp->range.first = f;
    exp->range.second = s;
    exp->range.last = l;
    return exp;
}

Exp* ast_expression_unary(Line ln, Token token, Exp* expression) {
    Exp* exp = exp_init(EXPRESSION_UNARY, ln);
    exp->unary.token = token;
    exp->unary.expression = expression;
    return exp;
}

Exp* ast_expression_binary(Line ln, Token token, Exp* l, Exp* r) {
    Exp* exp = exp_init(EXPRESSION_BINARY, ln);
    exp->binary.token = token;
    exp->binary.left_expression = l;
    exp->binary.right_expression = r;
    return exp;
}

Exp* ast_expression_cast(Line ln, Exp* cast, Type* type) {
    Exp* exp = exp_init(EXPRESSION_CAST, ln);
    exp->cast = cast;
    exp->type = type;
    // rearranging the list (only for arguments)
    exp->previous = cast->previous;
    cast->previous = NULL;
    exp->next = cast->next;
    cast->next = NULL;
    if (exp->previous) { exp->previous->next = exp; }
    if (exp->next)     { exp->next->previous = exp; }
    return exp;
}

// ==================================================
//
//  Function Call
//
// ==================================================

FC* ast_call(Line ln, Id* id, Exp* arguments) {
    FC* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_BASIC;
    function_call->line = ln;
    function_call->type = NULL;
    function_call->obj = NULL;
    function_call->id = id;
    function_call->arguments = arguments;
    function_call->argc = -1;
    function_call->fn = NULL;
    return function_call;
}

FC* ast_call_method(Line ln, Exp* i, Id* id, Exp* a) {
    FC* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_METHOD;
    function_call->line = ln;
    function_call->type = NULL;
    function_call->obj = i;
    function_call->id = id;
    function_call->arguments = a;
    function_call->argc = -1;
    function_call->fn = NULL;
    return function_call;
}

FC* ast_call_constructor(Line ln, Type* type, Exp* arguments) {
    FC* function_call;
    MALLOC(function_call, FunctionCall);
    function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
    function_call->line = ln;
    function_call->type = type;
    function_call->obj = NULL;
    function_call->id = NULL;
    function_call->arguments = arguments;
    function_call->argc = -1;
    function_call->fn = NULL;
    return function_call;
}
