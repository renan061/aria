#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove
#include <strings.h> // TODO: Remove

#include <llvm-c/Core.h>

#include "alloc.h"
#include "athreads.h"
#include "ir.h"
#include "ast.h"
#include "parser.h" // for the tokens

/*
 * TODO
 *  - Create macro for declares?
 *  - Create macro for function calls?
 *  - malloc error: mallocs que vc da na main podem ser perdidos quando a main
 *      termina e vc está usando esse malloc em outra thread?
 *  - free and destroy the mutex one day
 */

// TODO: move this elsewhere
#define FOREACH(Type, e, e0) for (Type* e = e0; e; e = e->next)

#define UNREACHABLE assert(NULL)    // TODO: Move this and look for assert(NULL)
#define TODO assert(NULL)           // TODO: Remove

#define STRUCTURE_MUTEX             (0)
#define STRUCTURE_VMT               (1)
#define STRUCTURE_ATTRIBUTE_START   (STRUCTURE_VMT + 1)

// TODO: Move
// Other names
#define NAME_THREAD_ARGUMENTS_STRUCTURE "_thread_arguments"
#define NAME_SPAWN_FUNCTION             "_spawn_block"

#define LLVM_GLOBAL_STRING "_global_string"

#define LABEL                   "label_"
#define LABEL_FUNCTION_ENTRY    LABEL "function_entry"
#define LABEL_IF_TRUE           LABEL "if_true"
#define LABEL_IF_END            LABEL "if_end"
#define LABEL_IF_ELSE_TRUE      LABEL "if_else_true"
#define LABEL_IF_ELSE_FALSE     LABEL "if_else_false"
#define LABEL_IF_ELSE_END       LABEL "if_else_end"
#define LABEL_WHILE             LABEL "while"
#define LABEL_WHILE_LOOP        LABEL "while_loop"
#define LABEL_WHILE_END         LABEL "while_end"
#define LABEL_FOR_INIT          LABEL "for_init"
#define LABEL_FOR_COND          LABEL "for_cond"
#define LABEL_FOR_LOOP          LABEL "for_loop"
#define LABEL_FOR_INC           LABEL "for_inc"
#define LABEL_FOR_END           LABEL "for_end"

// Native types
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// TODO: Move / rename -> see position_builder
static void state_close_block(IRState* state) {
    assert(state->block);
    state->block = NULL;
}

// LLVM (TODO: New Module?)
static LLVMTypeRef llvm_type(Type* type);
static LLVMTypeRef llvm_function_type(Definition*, size_t);
static void llvm_return(IRState*, LLVMValueRef);
static LLVMTypeRef llvm_structure(LLVMTypeRef[], size_t, const char*);

// Auxiliary
static LLVMValueRef stringliteral(IRState*, const char*);
static LLVMValueRef zerovalue(IRState*, Type*);

// TODO
static void backend_definition(IRState*, Definition*);
static void backend_block(IRState*, Block*);
static void backend_statement(IRState*, Statement*);
static void backend_capsa(IRState*, Capsa*);
static void backend_expression(IRState*, Expression*);
static void backend_condition(
    IRState*, Expression*, LLVMBasicBlockRef, LLVMBasicBlockRef
);
static LLVMValueRef backend_function_call(IRState*, FunctionCall*);

// ==================================================
//
//  Globals
//
// ==================================================

typedef struct Global Global;
struct Global {
    Global *previous, *next;
    Definition* value;
};

static Global* globals = NULL; // TODO: free this list

// adds global value to the globals linked list
static void addgval(Definition* value) {
    Global* global;
    MALLOC(global, Global);
    global->next = NULL;
    global->value = value;

    if (!globals) {
        globals = global;
        globals->previous = NULL;
    } else {
        // TODO: idiot, make it constant time
        Global* g = globals;
        for (; g->next; g = g->next);
        g->next = global;
        global->previous = g;
    }
}

// ==================================================
//
//  TODO: Misc
//
// ==================================================

static void position_builder(IRState* state, LLVMBasicBlockRef block) {
    assert(!state->block);
    LLVMPositionBuilderAtEnd(state->builder, block);
    state->block = block;
}

// TODO: Move to auxiliary area
// returns the mutex lock from a monitor
static LLVMValueRef monitormutex(LLVMBuilderRef builder, LLVMValueRef monitor) {
    return LLVMBuildLoad(
        builder,
        LLVMBuildStructGEP(builder, monitor, STRUCTURE_MUTEX, LLVM_TEMPORARY),
        LLVM_TEMPORARY
    );
}

// ==================================================
//
//  Implementation
//
// ==================================================

// TODO: Move / Rename
static void todospawn(IRState* state, FunctionCall* call) {
    // Defining the structure to be used for argument passing
    LLVMTypeRef fields[call->argument_count];
    unsigned int n = 0;
    for (Expression* e = call->arguments; e; e = e->next, n++) {
        fields[n] = llvm_type(e->type);
    }
    LLVMTypeRef type_structure = llvm_structure(
        fields, n, NAME_THREAD_ARGUMENTS_STRUCTURE
    );

    // Allocating memory for the structure
    LLVMValueRef structure = LLVMBuildMalloc(
        state->builder, type_structure, LLVM_TEMPORARY
    );

    // Filling the structure with values from the arguments
    n = 0;
    for (Expression* e = call->arguments; e; e = e->next, n++) {
        backend_expression(state, e);
        LLVMBuildStore(state->builder, e->llvm_value, LLVMBuildStructGEP(
            state->builder, structure, n, LLVM_TEMPORARY_NONE
        ));
    }

    // Defining the spawn function
    IRState* spawn_state = ir_state_new(state->module, state->builder);

    spawn_state->function = LLVMAddFunction(
        spawn_state->module, NAME_SPAWN_FUNCTION, ir_spawn_t
    );
    spawn_state->block = LLVMAppendBasicBlock(
        spawn_state->function, LABEL "spawn_function_entry"
    );
    LLVMPositionBuilderAtEnd(spawn_state->builder, spawn_state->block);

    // Parameters
    LLVMValueRef parameter = LLVMBuildBitCast(
        spawn_state->builder,
        LLVMGetParam(spawn_state->function, 0),
        LLVM_TYPE_POINTER(type_structure),
        LLVM_TEMPORARY
    );
    Definition* p = call->function_definition->function.parameters;
    for (unsigned int n = 0; p; p = p->next, n++) {
        p->capsa.capsa->llvm_value = LLVMBuildLoad(
            spawn_state->builder,
            LLVMBuildStructGEP(
                spawn_state->builder, parameter, n, LLVM_TEMPORARY
            ),
            LLVM_TEMPORARY
        );
    }

    // Block
    backend_block(spawn_state, call->function_definition->function.block);

    // TODO: Should free, but weird error (not this...)
    // LLVMBuildFree(state->builder, parameter);
    LLVMBuildRet(
        spawn_state->builder, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
    );

    // Going back to the original state builder position
    LLVMPositionBuilderAtEnd(state->builder, state->block);

    // Calling pthread_create
    ir_pthread_create(
        state->builder,
        spawn_state->function,
            LLVMBuildBitCast(
            state->builder, structure, LLVM_TYPE_POINTER_VOID, LLVM_TEMPORARY
        )
    );

    ir_state_free(spawn_state);
}

LLVMModuleRef backend_compile(AST* ast) {
    // setup
    __boolean = ast_type_boolean();
    __integer = ast_type_integer();
    __float = ast_type_float();
    __string = ast_type_string();
    __condition_queue = ast_type_condition_queue();

    // LLVM setup
    IRState* state = ir_state_new(
        LLVMModuleCreateWithName("main.aria"),
        LLVMCreateBuilder()
    );

    // includes
    ir_setup(state->module);
    ir_pthread_setup(state->module);

    // IR
    for (Definition* d = ast->definitions; d; d = d->next) {
        backend_definition(state, d);
    }

    // teardown
    ir_state_done(state);
    LLVMModuleRef module = state->module;
    ir_state_free(state);
    return module;
}

// ==================================================
//
//  Definition
//
// ==================================================

static void backend_definition_capsa(IRState*, Definition*);
static void backend_definition_function(IRState*, Definition*);
static void backend_definition_method(IRState*, Definition*);
static void backend_definition_constructor(IRState*, Definition*);
static void backend_definition_structure(IRState*, Definition*);
static void backend_definition_monitor(IRState*, Definition*);

static void initializefunction(IRState*, Definition*);
static void initializeglobals(IRState*);
static void initializevmt(IRState*, Definition*);

static void backend_definition(IRState* irs, Definition* def) {
    switch (def->tag) {
    case DEFINITION_CAPSA:
        backend_definition_capsa(irs, def);
        break;
    case DEFINITION_FUNCTION:
        backend_definition_function(irs, def);
        break;
    case DEFINITION_METHOD:
        backend_definition_method(irs, def);
        break;
    case DEFINITION_CONSTRUCTOR:
        backend_definition_constructor(irs, def);
        break;
    case DEFINITION_TYPE:
        switch (def->type->tag) {
        case TYPE_INTERFACE:
            break;
        case TYPE_STRUCTURE:
            backend_definition_structure(irs, def);
            break;
        case TYPE_MONITOR:
            backend_definition_monitor(irs, def);
            break;
        default:
            UNREACHABLE;
        }
        break;
    default:
        UNREACHABLE;
    }
}

static void backend_definition_capsa(IRState* irs, Definition* def) {
    Capsa* capsa = def->capsa.capsa;
    Expression* exp = def->capsa.expression;

    if (capsa->global) {
        addgval(def);
        return;
    }

    // TODO: Refactor
    // TODO: is this to detect if the capsa is inside a structure?
    if (capsa->llvm_structure_index > -1) {
        return;
    }

    // common scoped variables
    if (capsa->value) { // values
        backend_expression(irs, exp);
        capsa->llvm_value = exp->llvm_value;
    } else { // variables
        capsa->llvm_value = LLVMBuildAlloca(
            irs->builder, llvm_type(capsa->type), capsa->id->name
        );
        if (exp) {
            backend_expression(irs, exp);
            LLVMBuildStore(
                irs->builder, exp->llvm_value, capsa->llvm_value
            );
        }
    }
}

static void backend_definition_function(IRState* irs, Definition* def) {
    initializefunction(irs, def);

    if (def->function.id->name == scanner_native[SCANNER_NATIVE_MAIN]) {
        irs->main = true;
        initializeglobals(irs);
    }

    backend_block(irs, def->function.block);

    // implicit return
    if (irs->block) {
        llvm_return(irs, zerovalue(irs, def->function.type));
    }

    irs->main = false;
}

static void backend_definition_method(IRState* irs, Definition* def) {
    initializefunction(irs, def);

    // self is the first parameter
    irs->self = def->function.parameters->capsa.capsa->llvm_value;

    backend_block(irs, def->function.block);

    // implicit return
    if (irs->block) {
        llvm_return(irs, zerovalue(irs, def->function.type));
    }

    irs->self = NULL;
}

static void backend_definition_constructor(IRState* irs, Definition* def) {
    irs->initializer = true;

    initializefunction(irs, def);

    // creates the <self> instance
    LLVMTypeRef self_llvm_type = def->function.type->llvm_type;
    irs->self = LLVMBuildMalloc(irs->builder, self_llvm_type, LLVM_TEMPORARY);

    // creates the monitor's mutex
    LLVMValueRef mutex = ir_malloc(irs->builder, sizeof(pthread_mutex_t));
    ir_pthread_mutex_init(irs->builder, mutex);
    LLVMBuildStore(irs->builder, mutex, LLVMBuildStructGEP(
        irs->builder, irs->self, STRUCTURE_MUTEX, LLVM_TEMPORARY
    ));

    // creates the structure's VMT
    initializevmt(irs, def);

    // initializes attributes
    FOREACH(Definition, d, def->function.type->structure.definitions) {
        if (d->tag != DEFINITION_CAPSA) {
            continue;
        }
        backend_capsa(irs, d->capsa.capsa);
        LLVMValueRef expression;
        if (d->capsa.expression) {
            backend_expression(irs, d->capsa.expression);
            expression = d->capsa.expression->llvm_value;
        } else {
            expression = LLVMGetUndef(llvm_type(d->capsa.capsa->type));
        }
        LLVMBuildStore(irs->builder, expression, d->capsa.capsa->llvm_value);
    }

    backend_block(irs, def->function.block);

    // FIXME: why?
    if (irs->block) {
        llvm_return(irs, irs->self);
    }

    irs->self = NULL;
    irs->initializer = false;
}

static void backend_definition_structure(IRState* irs, Definition* def) {
    UNREACHABLE; // TODO
}

static void backend_definition_monitor(IRState* irs, Definition* def) {
    size_t attributes_size = def->type->structure.attributes_size;
    LLVMTypeRef attributes[STRUCTURE_ATTRIBUTE_START + attributes_size];

    // monitor's mutex
    attributes[STRUCTURE_MUTEX] = LLVM_TYPE_POINTER_PTHREAD_MUTEX_T;
    // attributes[STRUCTURE_VMT] = LLVM_TYPE_POINTER(LLVM_TYPE_POINTER_VOID);
    attributes[STRUCTURE_VMT] = LLVM_TYPE_POINTER(LLVMArrayType(
        LLVM_TYPE_POINTER_VOID, def->type->structure.methods_size
    ));

    // attributes
    for (int i = 0; i < def->type->structure.attributes_size; i++) {
        Capsa* capsa = def->type->structure.attributes[i]->capsa.capsa;
        capsa->llvm_structure_index = STRUCTURE_ATTRIBUTE_START + i;
        attributes[STRUCTURE_ATTRIBUTE_START + i] = llvm_type(capsa->type);
    }

    def->type->llvm_type = llvm_structure(
        attributes,
        def->type->structure.attributes_size + STRUCTURE_ATTRIBUTE_START,
        def->type->structure.id->name
    );

    for (int i = 0; i < def->type->structure.methods_size; i++) {
        Definition* function = def->type->structure.methods[i]; // FIXME
        backend_definition(irs, function);
    }
    backend_definition(irs, def->type->structure.constructor);

    // %array = alloca i8*
    // %pointer = bitcast i8* (%Monitor*)* @string to i8*
    // store i8* %pointer, i8** %array
    // %pointer2 = load i8*, i8** %array
    // %func = bitcast i8* %pointer2 to i8* (%Monitor*)*
    // %2 = call i8* %func(%Monitor* %0)
}

// -----------------------------------------------------------------------------

// auxiliary
// declares a function in LLVM
// links parameters' LLVM values inside the AST
// creates the ENTRY blocks and positions the instruction builder
// defines (def->llvm_value) and (irs->function)
static void initializefunction(IRState* irs, Definition* def) {
    assert(def->function.id);
    assert(def->function.type);

    // counts the number of parameters
    size_t parameters_size = 0;
    FOREACH(Definition, p, def->function.parameters) {
        parameters_size++;
    }

    // creates the function prototype 
    LLVMValueRef fn = LLVMAddFunction(
        irs->module,
        def->function.id->name,
        llvm_function_type(def, parameters_size)
    );

    // assigns LLVM values to each parameter
    int i = 0;
    FOREACH(Definition, p, def->function.parameters) {
        p->capsa.capsa->llvm_value = LLVMGetParam(fn, i++);
    }

    position_builder(irs, LLVMAppendBasicBlock(fn, LABEL_FUNCTION_ENTRY));

    def->llvm_value = fn;
    irs->function = fn;
}

// auxiliary - initializes all global values
static void initializeglobals(IRState* irs) {
    FOREACH(Global, global, globals) {
        Capsa* capsa = global->value->capsa.capsa;
        Expression* expression = global->value->capsa.expression;
        LLVMTypeRef type = llvm_type(capsa->type);
        capsa->llvm_value = LLVMAddGlobal(irs->module, type, capsa->id->name);
        LLVMSetInitializer(capsa->llvm_value, LLVMGetUndef(type));
        backend_expression(irs, expression);
        LLVMBuildStore(irs->builder, expression->llvm_value, capsa->llvm_value);
    }
}

static void initializevmt(IRState* irs, Definition* def) {
    Definition** methods = def->function.type->structure.methods;
    size_t methods_size = def->function.type->structure.methods_size;

    // allocates memory for the VMT
    LLVMTypeRef vmt_type = LLVMArrayType(LLVM_TYPE_POINTER_VOID, methods_size);
    LLVMValueRef vmt = LLVMBuildBitCast(
        irs->builder,
        LLVMBuildArrayMalloc(
            irs->builder,
            LLVM_TYPE_POINTER_VOID,
            LLVM_CONSTANT_INTEGER(methods_size),
            LLVM_TEMPORARY_VMT
        ),
        LLVM_TYPE_POINTER(vmt_type),
        LLVM_TEMPORARY_VMT
    );

    // fills VMT with the structure's methods
    for (int i = 0; i < methods_size; i++) {
        LLVMValueRef
            indices[2] = {LLVM_CONSTANT_INTEGER(0), LLVM_CONSTANT_INTEGER(i)},
            ptr = LLVMBuildGEP(irs->builder, vmt, indices, 2, LLVM_TEMPORARY),
            val = LLVMBuildBitCast(
                irs->builder,
                methods[i]->llvm_value,
                LLVM_TYPE_POINTER_VOID,
                LLVM_TEMPORARY
            )
        ;
        LLVMBuildStore(irs->builder, val, ptr);
    }

    // assigns the VMT to the <self> instance
    LLVMBuildStore(irs->builder, vmt, LLVMBuildStructGEP(
        irs->builder, irs->self, STRUCTURE_VMT, LLVM_TEMPORARY_VMT
    ));
}

// ==================================================
//
//  Block
//
// ==================================================

static void backend_block(IRState* irs, Block* block) {
    assert(block->tag == BLOCK);
    FOREACH(Block, b, block->next) {
        switch (b->tag) {
        case BLOCK_DEFINITION:
            FOREACH(Definition, d, b->definition) {
                backend_definition(irs, d);
            }
            break;
        case BLOCK_STATEMENT:
            backend_statement(irs, b->statement);
            break;
        default:
            UNREACHABLE;
        }
    }
}

// ==================================================
//
//  TODO
//
// ==================================================

static void backend_statement(IRState* state, Statement* statement) {
    switch (statement->tag) {
    case STATEMENT_ASSIGNMENT:
        backend_capsa(state, statement->assignment.capsa);
        backend_expression(state, statement->assignment.expression);
        LLVMBuildStore(
            state->builder,
            statement->assignment.expression->llvm_value,
            statement->assignment.capsa->llvm_value
        );
        break;
    case STATEMENT_FUNCTION_CALL:
        backend_function_call(state, statement->function_call);
        break;
    case STATEMENT_WAIT_FOR_IN: {
        LLVMBasicBlockRef
            bw = LLVMAppendBasicBlock(state->function, LABEL_WHILE),
            bl = LLVMAppendBasicBlock(state->function, LABEL_WHILE_LOOP),
            be = LLVMAppendBasicBlock(state->function, LABEL_WHILE_END);
        LLVMBuildBr(state->builder, bw);
        state_close_block(state);
        // While
        position_builder(state, bw);
        backend_condition(state, statement->wait_for_in.condition, be, bl);
        // Loop
        position_builder(state, bl);
        backend_expression(state, statement->wait_for_in.queue);
        LLVMValueRef
            mutex = monitormutex(state->builder, state->self),
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                state->builder,
                statement->wait_for_in.queue->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_wait(state->builder, cond, mutex);
        LLVMBuildBr(state->builder, bw);
        state_close_block(state);
        // End
        position_builder(state, be);
        break;
    }
    case STATEMENT_SIGNAL: {
        backend_expression(state, statement->signal);
        LLVMValueRef
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                state->builder,
                statement->signal->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_signal(state->builder, cond);
        break;
    }
    case STATEMENT_BROADCAST:
        backend_expression(state, statement->broadcast);
        LLVMValueRef
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                state->builder,
                statement->broadcast->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_broadcast(state->builder, cond);
        break;
    case STATEMENT_RETURN:
        if (statement->return_) {
            backend_expression(state, statement->return_);
            llvm_return(state, statement->return_->llvm_value);
        } else {
            llvm_return(state, NULL);
        }
        break;
    case STATEMENT_IF: {
        LLVMBasicBlockRef
            bt = LLVMAppendBasicBlock(state->function, LABEL_IF_TRUE),
            be = LLVMAppendBasicBlock(state->function, LABEL_IF_END);
        backend_condition(state, statement->if_.expression, bt, be);
        // If
        position_builder(state, bt);
        backend_block(state, statement->if_.block);
        if (state->block) {
            LLVMBuildBr(state->builder, be);
            state_close_block(state);
        }
        // End
        position_builder(state, be);
        break;
    }
    case STATEMENT_IF_ELSE: {
        LLVMBasicBlockRef
            bt = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_TRUE),
            bf = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_FALSE),
            be = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_END);
        backend_condition(state, statement->if_else.expression, bt, bf);
        // If
        position_builder(state, bt);
        backend_block(state, statement->if_else.if_block);
        if (state->block) {
            LLVMBuildBr(state->builder, be);
            state_close_block(state);
        }
        // Else
        position_builder(state, bf);
        backend_block(state, statement->if_else.else_block);
        if (state->block) {
            LLVMBuildBr(state->builder, be);
            state_close_block(state);
        }
        // End
        position_builder(state, be);
        break;
    }
    case STATEMENT_WHILE: {
        LLVMBasicBlockRef
            bw = LLVMAppendBasicBlock(state->function, LABEL_WHILE),
            bl = LLVMAppendBasicBlock(state->function, LABEL_WHILE_LOOP),
            be = LLVMAppendBasicBlock(state->function, LABEL_WHILE_END);
        LLVMBuildBr(state->builder, bw);
        state_close_block(state);
        // While
        position_builder(state, bw);
        backend_condition(state, statement->while_.expression, bl, be);
        // Loop
        position_builder(state, bl);
        backend_block(state, statement->while_.block);
        if (state->block) {
            LLVMBuildBr(state->builder, bw);
            state_close_block(state);
        }
        // End
        position_builder(state, be);
        break;
    }
    case STATEMENT_FOR: {
        // TODO: Check this later, better way to do this?
        LLVMBasicBlockRef
            binit = LLVMAppendBasicBlock(state->function, LABEL_FOR_INIT),
            bcond = LLVMAppendBasicBlock(state->function, LABEL_FOR_COND),
            bloop = LLVMAppendBasicBlock(state->function, LABEL_FOR_LOOP),
            binc  = LLVMAppendBasicBlock(state->function, LABEL_FOR_INC),
            bend  = LLVMAppendBasicBlock(state->function, LABEL_FOR_END);
        LLVMBuildBr(state->builder, binit);
        state_close_block(state);
        // init
        position_builder(state, binit);
        backend_definition(state, statement->for_.initialization);
        LLVMBuildBr(state->builder, bcond);
        state_close_block(state);
        // cond
        position_builder(state, bcond);
        backend_condition(state, statement->for_.condition, bloop, bend);
        // loop
        position_builder(state, bloop);
        backend_block(state, statement->for_.block);
        if (state->block) {
            LLVMBuildBr(state->builder, binc);
            state_close_block(state);
        }
        // inc
        position_builder(state, binc);
        backend_statement(state, statement->for_.increment);
        LLVMBuildBr(state->builder, bcond);
        state_close_block(state);
        // end
        position_builder(state, bend);
        break;
    }
    case STATEMENT_SPAWN: {
        todospawn(state, statement->spawn);
        break;
    }
    case STATEMENT_BLOCK:
        backend_block(state, statement->block);
        break;
    default:
        UNREACHABLE;
    }
}

static void backend_capsa(IRState* state, Capsa* capsa) {
    switch (capsa->tag) {
    case CAPSA_ID:
        // llvm_value dealt with already, unless...
        if (capsa->llvm_structure_index > -1) { // attributes
            assert(state->self);
            capsa->llvm_value = LLVMBuildStructGEP(
                state->builder,
                state->self,
                capsa->llvm_structure_index,
                LLVM_TEMPORARY
            );
        }
        break;
    case CAPSA_INDEXED: {
        backend_expression(state, capsa->indexed.array);
        backend_expression(state, capsa->indexed.index);
        LLVMValueRef indices[1] = {capsa->indexed.index->llvm_value};
        capsa->llvm_value = LLVMBuildGEP(
            state->builder,
            capsa->indexed.array->llvm_value,
            indices,
            1,
            LLVM_TEMPORARY
        );
        break;
    }
    default:
        UNREACHABLE;
    }
}

static void backend_expression(IRState* state, Expression* expression) {
    switch (expression->tag) {
    case EXPRESSION_LITERAL_BOOLEAN:
        expression->llvm_value =
            LLVM_CONSTANT_BOOLEAN(expression->literal.boolean);
        break;
    case EXPRESSION_LITERAL_INTEGER:
        expression->llvm_value =
            LLVM_CONSTANT_INTEGER(expression->literal.integer);
        break;
    case EXPRESSION_LITERAL_FLOAT:
        expression->llvm_value = LLVM_CONSTANT_FLOAT(expression->literal.float_);
        break;
    case EXPRESSION_LITERAL_STRING:
        expression->llvm_value = stringliteral(
            state, expression->literal.string
        );
        break;
    case EXPRESSION_LITERAL_ARRAY: {
        // evaluating the expressions inside the array
        int n = 0;
        for (Expression* e = expression->literal.array; e; e = e->next, n++) {
            backend_expression(state, e);
        }
        // allocating memory for the array
        LLVMValueRef size = LLVM_CONSTANT_INTEGER(n);
        expression->llvm_value = LLVMBuildArrayMalloc(
            state->builder,
            llvm_type(expression->type->array),
            size,
            LLVM_TEMPORARY
        );
        // setting the values inside the array
        n = 0;
        for (Expression* e = expression->literal.array; e; e = e->next) {
            LLVMValueRef
                indices[1] = {LLVM_CONSTANT_INTEGER(n++)},
                pointer = LLVMBuildGEP(
                    state->builder,
                    expression->llvm_value,
                    indices,
                    1,
                    LLVM_TEMPORARY
                )
            ;
            LLVMBuildStore(state->builder, e->llvm_value, pointer);
        }
        break;
    }
    case EXPRESSION_CAPSA:
        backend_capsa(state, expression->capsa);
        // TODO: Find better way to write this
        if (expression->capsa->llvm_structure_index > -1) { // attributes
            expression->llvm_value = LLVMBuildLoad(
                state->builder,
                expression->capsa->llvm_value,
                LLVM_TEMPORARY
            );
        } else if (expression->capsa->value && !expression->capsa->global) {
            // local values
            expression->llvm_value = expression->capsa->llvm_value;
        } else { // variables
            expression->llvm_value = LLVMBuildLoad(
                state->builder,
                expression->capsa->llvm_value,
                LLVM_TEMPORARY
            );
        }
        break;
    case EXPRESSION_FUNCTION_CALL:
        expression->llvm_value = backend_function_call(
            state, expression->function_call
        );
        break;
    case EXPRESSION_UNARY:
        switch (expression->unary.token) {
        case '-':
            backend_expression(state, expression->unary.expression);
            if (expression->type == __integer) {
                expression->llvm_value = LLVMBuildNeg(state->builder,
                    expression->unary.expression->llvm_value, LLVM_TEMPORARY);
            } else if (expression->type == __float) {
                expression->llvm_value = LLVMBuildFNeg(state->builder,
                    expression->unary.expression->llvm_value, LLVM_TEMPORARY);
            } else {
                UNREACHABLE;
            }
            break;
        case TK_NOT:
            goto CONDITION_EXPRESSION;
        default:
            UNREACHABLE;
        }
        break;
    case EXPRESSION_BINARY:
        switch (expression->binary.token) {
        case TK_OR:
        case TK_AND:
        case TK_EQUAL:
        case TK_NEQUAL:
        case TK_LEQUAL:
        case TK_GEQUAL:
        case '<':
        case '>':
            goto CONDITION_EXPRESSION;

        // Macro to be used by the [+, -, *, /] operations
        #define BINARY_ARITHMETICS(e, s, ifunc, ffunc) \
            if (e->type == __integer) { \
                e->llvm_value = ifunc(s->builder, \
                    e->binary.left_expression->llvm_value, \
                    e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
            } else if (e->type == __float) { \
                e->llvm_value = ffunc(s->builder, \
                    e->binary.left_expression->llvm_value, \
                    e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
            } else { \
                UNREACHABLE; \
            } \
        // End macro

        case '+':
            backend_expression(state, expression->binary.left_expression);
            backend_expression(state, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, state, LLVMBuildAdd, LLVMBuildFAdd);
            break;
        case '-':
            backend_expression(state, expression->binary.left_expression);
            backend_expression(state, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, state, LLVMBuildSub, LLVMBuildFSub);
            break;
        case '*':
            backend_expression(state, expression->binary.left_expression);
            backend_expression(state, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, state, LLVMBuildMul, LLVMBuildFMul);
            break;
        case '/':
            backend_expression(state, expression->binary.left_expression);
            backend_expression(state, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, state, LLVMBuildSDiv, LLVMBuildFDiv);
            break;
        default:
            UNREACHABLE;
        }
        break;
    case EXPRESSION_CAST:
        backend_expression(state, expression->cast);
        if (expression->cast->type == __integer &&
            expression->type == __float) {
            // Integer to Float
            expression->llvm_value = LLVMBuildSIToFP(
                state->builder,
                expression->cast->llvm_value,
                llvm_type(expression->type),
                LLVM_TEMPORARY
            );
        } else if (expression->cast->type == __float &&
            expression->type == __integer) {
            // Float to Integer
            expression->llvm_value = LLVMBuildFPToSI(
                state->builder,
                expression->cast->llvm_value,
                llvm_type(expression->type),
                LLVM_TEMPORARY
            );
        } else {
            UNREACHABLE;
        }
        break;
    default:
        UNREACHABLE;
    }

    return;

    CONDITION_EXPRESSION: { // expression ? true : false
        LLVMBasicBlockRef
            block_true  = LLVMAppendBasicBlock(state->function, LABEL "a"),
            block_false = LLVMAppendBasicBlock(state->function, LABEL "b"),
            block_phi   = LLVMAppendBasicBlock(state->function, LABEL "phi")
        ;

        backend_condition(state, expression, block_true, block_false);
        position_builder(state, block_true);
        LLVMBuildBr(state->builder, block_phi);
        state_close_block(state);
        position_builder(state, block_false);
        LLVMBuildBr(state->builder, block_phi);
        state_close_block(state);
        position_builder(state, block_phi);

        LLVMValueRef
            phi = LLVMBuildPhi(
                state->builder, LLVM_TYPE_BOOLEAN, LLVM_TEMPORARY_PHI
            ),
            incoming_values[2] = {LLVM_CONSTANT_TRUE, LLVM_CONSTANT_FALSE}
        ;
        LLVMBasicBlockRef incoming_blocks[2] = {block_true, block_false};
        LLVMAddIncoming(phi, incoming_values, incoming_blocks, 2);

        expression->llvm_value = phi;
    }
}

static void backend_condition(IRState* state, Expression* expression,
    LLVMBasicBlockRef lt, LLVMBasicBlockRef lf) {

    switch (expression->tag) {
    case EXPRESSION_LITERAL_BOOLEAN:
    case EXPRESSION_LITERAL_INTEGER:
    case EXPRESSION_LITERAL_FLOAT:
    case EXPRESSION_LITERAL_STRING:
    case EXPRESSION_CAPSA:
    case EXPRESSION_FUNCTION_CALL:
        goto EXPRESSION_CONDITION;
    case EXPRESSION_UNARY:
        switch (expression->unary.token) {
        case TK_NOT:
            backend_condition(state, expression->unary.expression, lf, lt);
            break;
        case '-':
            goto EXPRESSION_CONDITION;
        default:
            UNREACHABLE;
        }
        break;
    case EXPRESSION_BINARY: {
        Expression* l = expression->binary.left_expression;
        Expression* r = expression->binary.right_expression;
        LLVMBasicBlockRef label; // used by "or" and "and"

        switch (expression->binary.token) {
        case TK_OR:
            label = LLVMAppendBasicBlock(state->function, LABEL "or");
            backend_condition(state, l, lt, label);
            position_builder(state, label);
            backend_condition(state, r, lt, lf);
            break;
        case TK_AND:
            label = LLVMAppendBasicBlock(state->function, LABEL "and");
            backend_condition(state, l, label, lf);
            position_builder(state, label);
            backend_condition(state, r, lt, lf);
            break;
        case TK_EQUAL:
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntEQ, LLVMRealOEQ, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case TK_NEQUAL:
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntNE, LLVMRealONE, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case TK_LEQUAL:
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntSLE, LLVMRealOLE, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case TK_GEQUAL:
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntSGE, LLVMRealOGE, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case '<':
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntSLT, LLVMRealOLT, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case '>':
            backend_expression(state, l);
            backend_expression(state, r);
            expression->llvm_value = ir_cmp(
                state->builder, LLVMIntSGT, LLVMRealOGT, l, r
            );
            LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
            state_close_block(state);
            break;
        case '+':
        case '-':
        case '*':
        case '/':
            goto EXPRESSION_CONDITION;
        default:
            UNREACHABLE;
        }
        break;
    }
    case EXPRESSION_CAST:
        goto EXPRESSION_CONDITION;
    default:
        UNREACHABLE;
    }

    return;

    EXPRESSION_CONDITION: {
        backend_expression(state, expression);
        LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
        state_close_block(state);
    }
}

// ==================================================
//
//  FunctionCall
//
// ==================================================

static LLVMValueRef backend_fc_basic(IRState*, FunctionCall*);
static LLVMValueRef backend_fc_method(IRState*, FunctionCall*);
static LLVMValueRef backend_fc_condition_queue(IRState*, FunctionCall*);
static LLVMValueRef backend_fc_array(IRState*, FunctionCall*);

static LLVMValueRef* fcarguments(IRState*, FunctionCall*);

static LLVMValueRef backend_function_call(IRState* irs, FunctionCall* fc) {
    switch (fc->tag) {
    case FUNCTION_CALL_BASIC:
        return backend_fc_basic(irs, fc);
    case FUNCTION_CALL_METHOD:
        return backend_fc_method(irs, fc);
    case FUNCTION_CALL_CONSTRUCTOR:
        switch (fc->type->tag) {
        case TYPE_ID: // ConditionQueue initializer
            return backend_fc_condition_queue(irs, fc);
        case TYPE_ARRAY: // new array
            return backend_fc_array(irs, fc);
        case TYPE_MONITOR: // monitor constructor
            return backend_fc_basic(irs, fc);
        default:
            UNREACHABLE;
        }
        break;
    default:
        UNREACHABLE;
    }
}

static LLVMValueRef backend_fc_basic(IRState* irs, FunctionCall* fc) {
    LLVMValueRef* arguments = fcarguments(irs, fc);
    LLVMValueRef llvm_value = NULL;
    
    if (!fc->function_definition) {
        // FIXME: libraries & native functions
        if (!strcmp(fc->id->name, "print")) {
            llvm_value = ir_printf(irs->builder, arguments, fc->argument_count);
        }
    } else {
        llvm_value = LLVMBuildCall(
            irs->builder,
            fc->function_definition->llvm_value,
            arguments,
            fc->argument_count,
            LLVM_TEMPORARY_NONE
        );
    }
    free(arguments);
    return llvm_value;
}

// TODO: move
static LLVMValueRef ir_structure_vmt(LLVMBuilderRef B, LLVMValueRef self) {
    LLVMValueRef p = LLVMBuildStructGEP(B, self, STRUCTURE_VMT, LLVM_TEMPORARY);
    return LLVMBuildLoad(B, p, LLVM_TEMPORARY_VMT);
}

// TODO: move
// returns (*ptr)[i], given <ptr> is a pointer to an array
static LLVMValueRef ir_array_get(LLVMBuilderRef B, LLVMValueRef ptr, int i) {
    LLVMValueRef indices[2];
    indices[0] = LLVM_CONSTANT_INTEGER(0);
    indices[1] = LLVM_CONSTANT_INTEGER(i);
    ptr = LLVMBuildGEP(B, ptr, indices, 2, LLVM_TEMPORARY);
    return LLVMBuildLoad(B, ptr, LLVM_TEMPORARY);
}

static LLVMValueRef backend_fc_method(IRState* irs, FunctionCall* fc) {
    LLVMValueRef* arguments = fcarguments(irs, fc);
    LLVMValueRef self = arguments[0];

    LLVMValueRef mutex = monitormutex(irs->builder, self);
    ir_pthread_mutex_lock(irs->builder, mutex);

    int vmt_index = fc->function_definition->function.vmt_index;

    // VMT function call
    LLVMValueRef vmt = ir_structure_vmt(irs->builder, self);
    LLVMValueRef fn = ir_array_get(irs->builder, vmt, vmt_index);
    // bitcast from i8* to (llvm_function_type)*
    LLVMTypeRef function_type = llvm_function_type(
        fc->function_definition, fc->argument_count
    );
    fn = LLVMBuildBitCast(
        irs->builder, fn, LLVM_TYPE_POINTER(function_type), LLVM_TEMPORARY
    );

    LLVMValueRef llvm_value = LLVMBuildCall(
        irs->builder, fn, arguments, fc->argument_count, LLVM_TEMPORARY_NONE
    );

    ir_pthread_mutex_unlock(irs->builder, mutex);
    free(arguments);
    return llvm_value;
}

static LLVMValueRef backend_fc_condition_queue(IRState* irs, FunctionCall* fc) {
    assert(fc->type == __condition_queue);
    LLVMValueRef llvm_value = ir_malloc(irs->builder, sizeof(pthread_cond_t));
    ir_pthread_cond_init(irs->builder, llvm_value);
    return llvm_value;
}

static LLVMValueRef backend_fc_array(IRState* irs, FunctionCall* fc) {
    assert(fc->argument_count == 1);
    Expression* size = fc->arguments;
    backend_expression(irs, size);
    return LLVMBuildArrayMalloc(
        irs->builder,
        llvm_type(fc->type->array),
        size->llvm_value,
        LLVM_TEMPORARY
    );
}

// -----------------------------------------------------------------------------

// auxiliary
// returns an array with the function call's arguments
// must free the returned array
static LLVMValueRef* fcarguments(IRState* irs, FunctionCall* fc) {
    assert(fc->argument_count >= 0);
    FOREACH(Expression, e, fc->arguments) {
        backend_expression(irs, e);
    }
    LLVMValueRef* arguments;
    MALLOC_ARRAY(arguments, LLVMValueRef, fc->argument_count);
    size_t n = 0;
    FOREACH(Expression, e, fc->arguments) {
        arguments[n++] = e->llvm_value;
    }
    return arguments;
}

// ==================================================
//
//  LLVM
//
// ==================================================

static LLVMTypeRef llvm_type(Type* type) {
    assert(type);

    switch (type->tag) {
    case TYPE_VOID:
        return LLVM_ARIA_TYPE_VOID;
    case TYPE_ID:
        if (type == __boolean) {
            return LLVM_ARIA_TYPE_BOOLEAN;
        }
        if (type == __integer) {
            return LLVM_ARIA_TYPE_INTEGER;
        }
        if (type == __float) {
            return LLVM_ARIA_TYPE_FLOAT;
        }
        if (type == __string) {
            return LLVM_ARIA_TYPE_STRING;
        }
        if (type == __condition_queue) {
            return LLVM_ARIA_TYPE_CONDITION_QUEUE;
        }
        UNREACHABLE;
    case TYPE_ARRAY:
        return LLVM_ARIA_TYPE_ARRAY(llvm_type(type->array));
    case TYPE_MONITOR:
        return LLVM_ARIA_TYPE_MONITOR(type->llvm_type);
    default:
        UNREACHABLE;
    }
}

// given a function definition, returns its LLVM type
static LLVMTypeRef llvm_function_type(Definition* def, size_t psize) {
    int i = 0;
    LLVMTypeRef ptypes[psize];
    FOREACH(Definition, p, def->function.parameters) {
        ptypes[i++] = llvm_type(p->capsa.capsa->type);
    }
    LLVMTypeRef rtype = llvm_type(def->function.type);
    return LLVMFunctionType(rtype, ptypes, psize, false);
}

// TODO: Rename this: backend_return?
static void llvm_return(IRState* state, LLVMValueRef llvm_value) {
    assert(state->block);

    // The main function always ends with a call to pthred_exit
    if (state->main) {
        ir_pthread_exit(state->builder);
        LLVMBuildRetVoid(state->builder);
        state_close_block(state);
        return;
    }

    // Initializers always returns the 'self' reference
    if (state->initializer) {
        assert(state->self);
        LLVMBuildRet(state->builder, state->self);
        state_close_block(state);
        return;
    }

    // Return for normal functions and methods
    if (llvm_value) {
        LLVMBuildRet(state->builder, llvm_value);
    } else {
        LLVMBuildRetVoid(state->builder);
    }
    state_close_block(state);
}

// TODO: This functions is wrong, should use LLVMConstString and cast to pointer
static LLVMValueRef stringliteral(IRState* state, const char* string) {
    // Global
    size_t len = strlen(string);
    LLVMValueRef llvm_global = LLVMAddGlobal(
        state->module,
        LLVMArrayType(LLVMInt8Type(), len + 1),
        LLVM_GLOBAL_STRING
    );
    LLVMSetInitializer(llvm_global, LLVMConstString(string, len, false));
    LLVMSetVisibility(llvm_global, LLVMHiddenVisibility);

    // Expression
    return LLVMBuildPointerCast(
        state->builder,
        llvm_global,
        LLVM_ARIA_TYPE_STRING,
        LLVM_TEMPORARY
    );
}

// TODO: Doc
static LLVMTypeRef llvm_structure(LLVMTypeRef fields[], size_t size,
    const char* name) {

    LLVMTypeRef type = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
    LLVMStructSetBody(type, fields, size, false); // TODO: Packed?
    return type;
}

// ==================================================
//
//  Auxiliary
//
// ==================================================

// TODO: Find a better way to write this...
// TODO: Read about llvm zeroinitalizer (ConstantAggregateZero ?)
static LLVMValueRef zerovalue(IRState* state, Type* type) {
    switch (type->tag) {
    case TYPE_VOID:
        return NULL;
    case TYPE_ID:
        if (type == __boolean) {
            return LLVM_CONSTANT_FALSE;
        }
        if (type == __integer) {
            return LLVM_CONSTANT_INTEGER(0);
        }
        if (type == __float) {
            return LLVM_CONSTANT_FLOAT(0);
        }
        if (type == __string) {
            return stringliteral(state, "");
        }
        if (type == __condition_queue) {
            return LLVMConstNull(LLVM_ARIA_TYPE_CONDITION_QUEUE);
        }
        UNREACHABLE;
    case TYPE_ARRAY:
        return LLVMConstNull(llvm_type(type));
    case TYPE_MONITOR:
        return LLVMConstNull(llvm_type(type));
    default:
        UNREACHABLE;
    }
}
