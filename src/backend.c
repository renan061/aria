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
static void state_close_block(IRState* irs) {
    assert(irs->block);
    irs->block = NULL;
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

static void position_builder(IRState* irs, LLVMBasicBlockRef block) {
    assert(!irs->block);
    LLVMPositionBuilderAtEnd(irs->B, block);
    irs->block = block;
}

// TODO: Move to auxiliary area
// returns the mutex lock from a monitor
static LLVMValueRef monitormutex(LLVMBuilderRef B, LLVMValueRef monitor) {
    LLVMValueRef x = LLVMBuildLoad(B,
        LLVMBuildStructGEP(B, monitor, STRUCTURE_MUTEX, LLVM_TEMPORARY),
        LLVM_TEMPORARY
    );
    return x;
}

// ==================================================
//
//  Implementation
//
// ==================================================

// TODO: move / rename / fix
static void todospawn(IRState* irs, FunctionCall* call) {
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
        irs->B, type_structure, LLVM_TEMPORARY
    );

    // Filling the structure with values from the arguments
    n = 0;
    for (Expression* e = call->arguments; e; e = e->next, n++) {
        backend_expression(irs, e);
        LLVMBuildStore(irs->B, e->llvm_value, LLVMBuildStructGEP(
            irs->B, structure, n, LLVM_TEMPORARY_NONE
        ));
    }

    // Defining the spawn function
    IRState* spawn_irs = ir_state_new(irs->M, irs->B);

    spawn_irs->function = LLVMAddFunction(
        spawn_irs->M, NAME_SPAWN_FUNCTION, ir_spawn_t
    );
    spawn_irs->block = LLVMAppendBasicBlock(
        spawn_irs->function, LABEL "spawn_function_entry"
    );
    LLVMPositionBuilderAtEnd(spawn_irs->B, spawn_irs->block);

    // Parameters
    LLVMValueRef parameter = LLVMBuildBitCast(
        spawn_irs->B,
        LLVMGetParam(spawn_irs->function, 0),
        LLVM_TYPE_POINTER(type_structure),
        LLVM_TEMPORARY
    );
    Definition* p = call->function_definition->function.parameters;
    for (unsigned int n = 0; p; p = p->next, n++) {
        p->capsa.capsa->llvm_value = LLVMBuildLoad(
            spawn_irs->B,
            LLVMBuildStructGEP(
                spawn_irs->B, parameter, n, LLVM_TEMPORARY
            ),
            LLVM_TEMPORARY
        );
    }

    // Block
    backend_block(spawn_irs, call->function_definition->function.block);

    // TODO: Should free, but weird error (not this...)
    // LLVMBuildFree(irs->B, parameter);
    LLVMBuildRet(
        spawn_irs->B, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
    );

    // Going back to the original irs builder position
    LLVMPositionBuilderAtEnd(irs->B, irs->block);

    // Calling pthread_create
    ir_pthread_create(
        irs->B,
        spawn_irs->function,
            LLVMBuildBitCast(
            irs->B, structure, LLVM_TYPE_POINTER_VOID, LLVM_TEMPORARY
        )
    );

    ir_state_free(spawn_irs);
}

LLVMModuleRef backend_compile(AST* ast) {
    // setup
    __boolean = ast_type_boolean();
    __integer = ast_type_integer();
    __float = ast_type_float();
    __string = ast_type_string();
    __condition_queue = ast_type_condition_queue();

    // LLVM setup
    IRState* irs = ir_state_new(
        LLVMModuleCreateWithName("main.aria"),
        LLVMCreateBuilder()
    );

    // includes
    ir_setup(irs->M);
    ir_pthread_setup(irs->M);

    // IR
    for (Definition* d = ast->definitions; d; d = d->next) {
        backend_definition(irs, d);
    }

    // teardown
    ir_state_done(irs);
    LLVMModuleRef M = irs->M;
    ir_state_free(irs);
    return M;
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
static void backend_definition_interface(IRState*, Definition*);
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
            backend_definition_interface(irs, def);
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

    if (capsa->global) {
        addgval(def);
        return;
    }

    // TODO: is this to detect if the capsa is a structure property? (refactor)
    if (capsa->llvm_structure_index > -1) {
        return;
    }

    // common scoped variables
    Expression* exp = def->capsa.expression;
    if (capsa->value) { // values
        backend_expression(irs, exp);
        capsa->llvm_value = exp->llvm_value;
    } else { // variables
        capsa->llvm_value = LLVMBuildAlloca(
            irs->B, llvm_type(capsa->type), capsa->id->name
        );
        if (exp) {
            backend_expression(irs, exp);
            LLVMBuildStore(
                irs->B, exp->llvm_value, capsa->llvm_value
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

    // <self> is the first parameter
    irs->self = def->function.parameters->capsa.capsa->llvm_value;
    irs->self = LLVMBuildBitCast(
        irs->B,
        irs->self,
        LLVM_TYPE_POINTER(irs->structure_type),
        LLVM_TEMPORARY
    );

    backend_block(irs, def->function.block);

    // implicit return
    if (irs->block) {
        llvm_return(irs, zerovalue(irs, def->function.type));
    }

    irs->self = NULL;
}

// TODO: move
static LLVMValueRef generic_pointer(LLVMBuilderRef B, LLVMValueRef ptr) {
    return LLVMBuildBitCast(B, ptr, LLVM_TYPE_POINTER_VOID, LLVM_TEMPORARY);
}

static void backend_definition_constructor(IRState* irs, Definition* def) {
    irs->initializer = true;

    initializefunction(irs, def);

    // creates the <self> instance
    irs->self = LLVMBuildMalloc(irs->B, irs->structure_type, LLVM_TEMPORARY);

    // creates the monitor's mutex
    LLVMValueRef mutex = ir_malloc(irs->B, sizeof(pthread_mutex_t));
    ir_pthread_mutex_init(irs->B, mutex);
    LLVMBuildStore(irs->B, mutex, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCTURE_MUTEX, LLVM_TEMPORARY
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
        LLVMBuildStore(irs->B, expression, d->capsa.capsa->llvm_value);
    }

    backend_block(irs, def->function.block);

    // TODO: why is this IF here? Should check the tests.
    if (irs->block) {
        llvm_return(irs, generic_pointer(irs->B, irs->self));
    }

    irs->self = NULL;
    irs->initializer = false;
}

static void backend_definition_interface(IRState* irs, Definition* def) {
    size_t vmt_size = def->type->structure.methods_size;
    LLVMTypeRef fields[2] = {
        LLVM_TYPE_POINTER_PTHREAD_MUTEX_T,
        LLVM_TYPE_POINTER(LLVMArrayType(LLVM_TYPE_POINTER_VOID, vmt_size))
    };
    const char* name = def->type->structure.id->name;
    def->type->llvm_type = llvm_structure(fields, 2, name);
}

static void backend_definition_structure(IRState* irs, Definition* def) {
    UNREACHABLE; // TODO
}

static void backend_definition_monitor(IRState* irs, Definition* def) {
    size_t attributes_size = def->type->structure.attributes_size;
    LLVMTypeRef attributes[STRUCTURE_ATTRIBUTE_START + attributes_size];

    // mutex & VMT
    attributes[STRUCTURE_MUTEX] = LLVM_TYPE_POINTER_PTHREAD_MUTEX_T;
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

    irs->structure_type = def->type->llvm_type;
    for (int i = 0; i < def->type->structure.methods_size; i++) {
        Definition* function = def->type->structure.methods[i]; // FIXME: ?
        backend_definition(irs, function);
    }
    backend_definition(irs, def->type->structure.constructor);
    irs->structure_type = NULL;
}

// -----------------------------------------------------------------------------

// auxiliary
// declares a function in LLVM
// links parameters' LLVM values inside the AST
// creates the ENTRY blocks and positions the instruction builder
// defines (f->llvm_value) and (irs->function)
static void initializefunction(IRState* irs, Definition* f) {
    assert(f->function.id);
    assert(f->function.type);

    // counts the number of parameters
    size_t parameters_size = 0;
    FOREACH(Definition, p, f->function.parameters) {
        parameters_size++;
    }

    // creates the function prototype 
    LLVMValueRef fn = LLVMAddFunction(
        irs->M,
        f->function.id->name,
        llvm_function_type(f, parameters_size)
    );

    { // assigns LLVM values to each parameter
        int i = 0;
        FOREACH(Definition, p, f->function.parameters) {
            p->capsa.capsa->llvm_value = LLVMGetParam(fn, i++);
        }
    }

    position_builder(irs, LLVMAppendBasicBlock(fn, LABEL_FUNCTION_ENTRY));

    f->llvm_value = fn;
    irs->function = fn;
}

// auxiliary - initializes all global values
static void initializeglobals(IRState* irs) {
    FOREACH(Global, global, globals) {
        Capsa* capsa = global->value->capsa.capsa;
        Expression* expression = global->value->capsa.expression;
        LLVMTypeRef type = llvm_type(capsa->type);
        capsa->llvm_value = LLVMAddGlobal(irs->M, type, capsa->id->name);
        LLVMSetInitializer(capsa->llvm_value, LLVMGetUndef(type));
        backend_expression(irs, expression);
        LLVMBuildStore(irs->B, expression->llvm_value, capsa->llvm_value);
    }
}

// auxiliary - creates the structure's VMT
static void initializevmt(IRState* irs, Definition* def) {
    Definition** methods = def->function.type->structure.methods;
    size_t methods_size = def->function.type->structure.methods_size;

    // allocates memory for the VMT
    LLVMTypeRef vmt_type = LLVMArrayType(LLVM_TYPE_POINTER_VOID, methods_size);
    LLVMValueRef vmt = LLVMBuildBitCast(
        irs->B,
        LLVMBuildArrayMalloc(
            irs->B,
            LLVM_TYPE_POINTER_VOID,
            LLVM_CONSTANT_INTEGER(methods_size),
            LLVM_TEMPORARY_VMT
        ),
        LLVM_TYPE_POINTER(vmt_type),
        LLVM_TEMPORARY_VMT
    );

    // fills the VMT with the structure's methods
    for (int i = 0; i < methods_size; i++) {
        LLVMValueRef
            indices[2] = {LLVM_CONSTANT_INTEGER(0), LLVM_CONSTANT_INTEGER(i)},
            ptr = LLVMBuildGEP(irs->B, vmt, indices, 2, LLVM_TEMPORARY),
            val = LLVMBuildBitCast(
                irs->B,
                methods[i]->llvm_value,
                LLVM_TYPE_POINTER_VOID,
                LLVM_TEMPORARY
            )
        ;
        LLVMBuildStore(irs->B, val, ptr);
    }

    // stores the VMT inside the <self> instance
    LLVMBuildStore(irs->B, vmt, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCTURE_VMT, LLVM_TEMPORARY_VMT
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

static void backend_statement(IRState* irs, Statement* statement) {
    switch (statement->tag) {
    case STATEMENT_ASSIGNMENT:
        backend_capsa(irs, statement->assignment.capsa);
        backend_expression(irs, statement->assignment.expression);
        LLVMBuildStore(
            irs->B,
            statement->assignment.expression->llvm_value,
            statement->assignment.capsa->llvm_value
        );
        break;
    case STATEMENT_FUNCTION_CALL:
        backend_function_call(irs, statement->function_call);
        break;
    case STATEMENT_WAIT_FOR_IN: {
        LLVMBasicBlockRef
            bw = LLVMAppendBasicBlock(irs->function, LABEL_WHILE),
            bl = LLVMAppendBasicBlock(irs->function, LABEL_WHILE_LOOP),
            be = LLVMAppendBasicBlock(irs->function, LABEL_WHILE_END);
        LLVMBuildBr(irs->B, bw);
        state_close_block(irs);
        // While
        position_builder(irs, bw);
        backend_condition(irs, statement->wait_for_in.condition, be, bl);
        // Loop
        position_builder(irs, bl);
        backend_expression(irs, statement->wait_for_in.queue);
        LLVMValueRef
            mutex = monitormutex(irs->B, irs->self),
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->wait_for_in.queue->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_wait(irs->B, cond, mutex);
        LLVMBuildBr(irs->B, bw);
        state_close_block(irs);
        // End
        position_builder(irs, be);
        break;
    }
    case STATEMENT_SIGNAL: {
        backend_expression(irs, statement->signal);
        LLVMValueRef
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->signal->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_signal(irs->B, cond);
        break;
    }
    case STATEMENT_BROADCAST:
        backend_expression(irs, statement->broadcast);
        LLVMValueRef
            indices[1] = {LLVM_CONSTANT_INTEGER(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->broadcast->llvm_value,
                indices,
                1,
                LLVM_TEMPORARY
            )
        ;
        ir_pthread_cond_broadcast(irs->B, cond);
        break;
    case STATEMENT_RETURN:
        if (statement->return_) {
            backend_expression(irs, statement->return_);
            llvm_return(irs, statement->return_->llvm_value);
        } else {
            llvm_return(irs, NULL);
        }
        break;
    case STATEMENT_IF: {
        LLVMBasicBlockRef
            bt = LLVMAppendBasicBlock(irs->function, LABEL_IF_TRUE),
            be = LLVMAppendBasicBlock(irs->function, LABEL_IF_END);
        backend_condition(irs, statement->if_.expression, bt, be);
        // If
        position_builder(irs, bt);
        backend_block(irs, statement->if_.block);
        if (irs->block) {
            LLVMBuildBr(irs->B, be);
            state_close_block(irs);
        }
        // End
        position_builder(irs, be);
        break;
    }
    case STATEMENT_IF_ELSE: {
        LLVMBasicBlockRef
            bt = LLVMAppendBasicBlock(irs->function, LABEL_IF_ELSE_TRUE),
            bf = LLVMAppendBasicBlock(irs->function, LABEL_IF_ELSE_FALSE),
            be = LLVMAppendBasicBlock(irs->function, LABEL_IF_ELSE_END);
        backend_condition(irs, statement->if_else.expression, bt, bf);
        // If
        position_builder(irs, bt);
        backend_block(irs, statement->if_else.if_block);
        if (irs->block) {
            LLVMBuildBr(irs->B, be);
            state_close_block(irs);
        }
        // Else
        position_builder(irs, bf);
        backend_block(irs, statement->if_else.else_block);
        if (irs->block) {
            LLVMBuildBr(irs->B, be);
            state_close_block(irs);
        }
        // End
        position_builder(irs, be);
        break;
    }
    case STATEMENT_WHILE: {
        LLVMBasicBlockRef
            bw = LLVMAppendBasicBlock(irs->function, LABEL_WHILE),
            bl = LLVMAppendBasicBlock(irs->function, LABEL_WHILE_LOOP),
            be = LLVMAppendBasicBlock(irs->function, LABEL_WHILE_END);
        LLVMBuildBr(irs->B, bw);
        state_close_block(irs);
        // While
        position_builder(irs, bw);
        backend_condition(irs, statement->while_.expression, bl, be);
        // Loop
        position_builder(irs, bl);
        backend_block(irs, statement->while_.block);
        if (irs->block) {
            LLVMBuildBr(irs->B, bw);
            state_close_block(irs);
        }
        // End
        position_builder(irs, be);
        break;
    }
    case STATEMENT_FOR: {
        // TODO: Check this later, better way to do this?
        LLVMBasicBlockRef
            binit = LLVMAppendBasicBlock(irs->function, LABEL_FOR_INIT),
            bcond = LLVMAppendBasicBlock(irs->function, LABEL_FOR_COND),
            bloop = LLVMAppendBasicBlock(irs->function, LABEL_FOR_LOOP),
            binc  = LLVMAppendBasicBlock(irs->function, LABEL_FOR_INC),
            bend  = LLVMAppendBasicBlock(irs->function, LABEL_FOR_END);
        LLVMBuildBr(irs->B, binit);
        state_close_block(irs);
        // init
        position_builder(irs, binit);
        backend_definition(irs, statement->for_.initialization);
        LLVMBuildBr(irs->B, bcond);
        state_close_block(irs);
        // cond
        position_builder(irs, bcond);
        backend_condition(irs, statement->for_.condition, bloop, bend);
        // loop
        position_builder(irs, bloop);
        backend_block(irs, statement->for_.block);
        if (irs->block) {
            LLVMBuildBr(irs->B, binc);
            state_close_block(irs);
        }
        // inc
        position_builder(irs, binc);
        backend_statement(irs, statement->for_.increment);
        LLVMBuildBr(irs->B, bcond);
        state_close_block(irs);
        // end
        position_builder(irs, bend);
        break;
    }
    case STATEMENT_SPAWN:
        todospawn(irs, statement->spawn);
        break;
    case STATEMENT_ACQUIRE_VALUE:
        // TODO: UNREACHABLE;
        break;
    case STATEMENT_BLOCK:
        backend_block(irs, statement->block);
        break;
    default:
        UNREACHABLE;
    }
}

static void backend_capsa(IRState* irs, Capsa* capsa) {
    switch (capsa->tag) {
    case CAPSA_ID:
        // llvm_value dealt with already, unless...
        if (capsa->llvm_structure_index > -1) { // attributes
            assert(irs->self);
            capsa->llvm_value = LLVMBuildStructGEP(
                irs->B, irs->self, capsa->llvm_structure_index, LLVM_TEMPORARY
            );
        }
        break;
    case CAPSA_INDEXED: {
        backend_expression(irs, capsa->indexed.array);
        backend_expression(irs, capsa->indexed.index);
        LLVMValueRef indices[1] = {capsa->indexed.index->llvm_value};
        capsa->llvm_value = LLVMBuildGEP(
            irs->B,
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

static void backend_expression(IRState* irs, Expression* expression) {
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
            irs, expression->literal.string
        );
        break;
    case EXPRESSION_LITERAL_ARRAY: {
        // evaluating the expressions inside the array
        int n = 0;
        for (Expression* e = expression->literal.array; e; e = e->next, n++) {
            backend_expression(irs, e);
        }
        // allocating memory for the array
        LLVMValueRef size = LLVM_CONSTANT_INTEGER(n);
        expression->llvm_value = LLVMBuildArrayMalloc(
            irs->B,
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
                    irs->B,
                    expression->llvm_value,
                    indices,
                    1,
                    LLVM_TEMPORARY
                )
            ;
            LLVMBuildStore(irs->B, e->llvm_value, pointer);
        }
        break;
    }
    case EXPRESSION_CAPSA:
        backend_capsa(irs, expression->capsa);
        // TODO: Find better way to write this
        if (expression->capsa->llvm_structure_index > -1) { // attributes
            expression->llvm_value = LLVMBuildLoad(
                irs->B,
                expression->capsa->llvm_value,
                LLVM_TEMPORARY
            );
        } else if (expression->capsa->value && !expression->capsa->global) {
            // local values
            expression->llvm_value = expression->capsa->llvm_value;
        } else { // variables
            expression->llvm_value = LLVMBuildLoad(
                irs->B,
                expression->capsa->llvm_value,
                LLVM_TEMPORARY
            );
        }
        break;
    case EXPRESSION_FUNCTION_CALL:
        expression->llvm_value = backend_function_call(
            irs, expression->function_call
        );
        break;
    case EXPRESSION_UNARY:
        switch (expression->unary.token) {
        case '-':
            backend_expression(irs, expression->unary.expression);
            if (expression->type == __integer) {
                expression->llvm_value = LLVMBuildNeg(irs->B,
                    expression->unary.expression->llvm_value, LLVM_TEMPORARY);
            } else if (expression->type == __float) {
                expression->llvm_value = LLVMBuildFNeg(irs->B,
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
                e->llvm_value = ifunc(s->B, \
                    e->binary.left_expression->llvm_value, \
                    e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
            } else if (e->type == __float) { \
                e->llvm_value = ffunc(s->B, \
                    e->binary.left_expression->llvm_value, \
                    e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
            } else { \
                UNREACHABLE; \
            } \
        // End macro

        case '+':
            backend_expression(irs, expression->binary.left_expression);
            backend_expression(irs, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, irs, LLVMBuildAdd, LLVMBuildFAdd);
            break;
        case '-':
            backend_expression(irs, expression->binary.left_expression);
            backend_expression(irs, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, irs, LLVMBuildSub, LLVMBuildFSub);
            break;
        case '*':
            backend_expression(irs, expression->binary.left_expression);
            backend_expression(irs, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, irs, LLVMBuildMul, LLVMBuildFMul);
            break;
        case '/':
            backend_expression(irs, expression->binary.left_expression);
            backend_expression(irs, expression->binary.right_expression);
            BINARY_ARITHMETICS(expression, irs, LLVMBuildSDiv, LLVMBuildFDiv);
            break;
        default:
            UNREACHABLE;
        }
        break;
    case EXPRESSION_CAST: {
        backend_expression(irs, expression->cast);
        Type* from = expression->cast->type;
        Type* to = expression->type;

        if (from == __integer && to == __float) {
            // Integer to Float
            expression->llvm_value = LLVMBuildSIToFP(
                irs->B,
                expression->cast->llvm_value,
                llvm_type(expression->type),
                LLVM_TEMPORARY
            );
        } else if (from == __float && to == __integer) {
            // Float to Integer
            expression->llvm_value = LLVMBuildFPToSI(
                irs->B,
                expression->cast->llvm_value,
                llvm_type(expression->type),
                LLVM_TEMPORARY
            );
        } else if (from->tag == TYPE_MONITOR && to->tag == TYPE_INTERFACE) {
            // Monitor to Interface
            expression->llvm_value = expression->cast->llvm_value;
        } else {
            UNREACHABLE;
        }
        break;
    }
    default:
        UNREACHABLE;
    }

    return;

    CONDITION_EXPRESSION: { // expression ? true : false
        LLVMBasicBlockRef
            block_true  = LLVMAppendBasicBlock(irs->function, LABEL "a"),
            block_false = LLVMAppendBasicBlock(irs->function, LABEL "b"),
            block_phi   = LLVMAppendBasicBlock(irs->function, LABEL "phi")
        ;

        backend_condition(irs, expression, block_true, block_false);
        position_builder(irs, block_true);
        LLVMBuildBr(irs->B, block_phi);
        state_close_block(irs);
        position_builder(irs, block_false);
        LLVMBuildBr(irs->B, block_phi);
        state_close_block(irs);
        position_builder(irs, block_phi);

        LLVMValueRef
            phi = LLVMBuildPhi(
                irs->B, LLVM_TYPE_BOOLEAN, LLVM_TEMPORARY_PHI
            ),
            incoming_values[2] = {LLVM_CONSTANT_TRUE, LLVM_CONSTANT_FALSE}
        ;
        LLVMBasicBlockRef incoming_blocks[2] = {block_true, block_false};
        LLVMAddIncoming(phi, incoming_values, incoming_blocks, 2);

        expression->llvm_value = phi;
    }
}

static void backend_condition(IRState* irs, Expression* expression,
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
            backend_condition(irs, expression->unary.expression, lf, lt);
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
            label = LLVMAppendBasicBlock(irs->function, LABEL "or");
            backend_condition(irs, l, lt, label);
            position_builder(irs, label);
            backend_condition(irs, r, lt, lf);
            break;
        case TK_AND:
            label = LLVMAppendBasicBlock(irs->function, LABEL "and");
            backend_condition(irs, l, label, lf);
            position_builder(irs, label);
            backend_condition(irs, r, lt, lf);
            break;
        case TK_EQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntEQ, LLVMRealOEQ, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
            break;
        case TK_NEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntNE, LLVMRealONE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
            break;
        case TK_LEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntSLE, LLVMRealOLE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
            break;
        case TK_GEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntSGE, LLVMRealOGE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
            break;
        case '<':
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntSLT, LLVMRealOLT, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
            break;
        case '>':
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->llvm_value = ir_cmp(
                irs->B, LLVMIntSGT, LLVMRealOGT, l, r
            );
            LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
            state_close_block(irs);
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
        backend_expression(irs, expression);
        LLVMBuildCondBr(irs->B, expression->llvm_value, lt, lf);
        state_close_block(irs);
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
            llvm_value = ir_printf(irs->B, arguments, fc->argument_count);
        }
    } else {
        llvm_value = LLVMBuildCall(
            irs->B,
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
    LLVMValueRef instance = LLVMBuildBitCast(
        irs->B,
        arguments[0],
        LLVM_TYPE_POINTER(fc->instance->type->llvm_type),
        LLVM_TEMPORARY
    );

    LLVMValueRef mutex = monitormutex(irs->B, instance);

    ir_pthread_mutex_lock(irs->B, mutex);

    int vmt_index = fc->function_definition->function.vmt_index;
    if (fc->instance->type->tag == TYPE_INTERFACE) { // FIXME: gambiarra
        vmt_index += 2; // accounting for the <unlocked> pair
    }

    // VMT function call
    LLVMValueRef vmt = ir_structure_vmt(irs->B, instance);
    LLVMValueRef fn = ir_array_get(irs->B, vmt, vmt_index);

    // bitcast from i8* to (llvm_function_type)*
    // FIXME
    // LLVMTypeRef function_type = llvm_function_type(
    //     fc->function_definition, fc->argument_count
    // );
    LLVMTypeRef function_type;
    {
        Definition* def = fc->function_definition;
        size_t psize = fc->argument_count;
        int i = 1;
        LLVMTypeRef ptypes[psize];
        ptypes[0] = LLVM_ARIA_TYPE_MONITOR; // TODO: TYPE_OBJECT (i8*)
        FOREACH(Definition, p, def->function.parameters->next) {
            ptypes[i++] = llvm_type(p->capsa.capsa->type);
        }
        LLVMTypeRef returntype = llvm_type(def->function.type);
        function_type = LLVMFunctionType(returntype, ptypes, psize, false);
    }
    fn = LLVMBuildBitCast(
        irs->B, fn, LLVM_TYPE_POINTER(function_type), LLVM_TEMPORARY
    );
    LLVMValueRef llvm_value = LLVMBuildCall(
        irs->B, fn, arguments, fc->argument_count, LLVM_TEMPORARY_NONE
    );

    ir_pthread_mutex_unlock(irs->B, mutex);
    free(arguments);

    return llvm_value;
}

static LLVMValueRef backend_fc_condition_queue(IRState* irs, FunctionCall* fc) {
    assert(fc->type == __condition_queue);
    LLVMValueRef llvm_value = ir_malloc(irs->B, sizeof(pthread_cond_t));
    ir_pthread_cond_init(irs->B, llvm_value);
    return llvm_value;
}

static LLVMValueRef backend_fc_array(IRState* irs, FunctionCall* fc) {
    assert(fc->argument_count == 1);
    Expression* size = fc->arguments;
    backend_expression(irs, size);
    return LLVMBuildArrayMalloc(
        irs->B,
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
    case TYPE_INTERFACE:
        return LLVM_ARIA_TYPE_INTERFACE;
    case TYPE_MONITOR:
        return LLVM_ARIA_TYPE_MONITOR;
    default:
        UNREACHABLE;
    }
}

// given a function definition, returns its LLVM type
static LLVMTypeRef llvm_function_type(Definition* f, size_t psize) {
    int i = 0;
    LLVMTypeRef ptypes[psize];
    FOREACH(Definition, p, f->function.parameters) {
        ptypes[i++] = llvm_type(p->capsa.capsa->type);
    }
    LLVMTypeRef returntype = llvm_type(f->function.type);
    return LLVMFunctionType(returntype, ptypes, psize, false);
}

// TODO: Rename this: backend_return?
static void llvm_return(IRState* irs, LLVMValueRef llvm_value) {
    assert(irs->block);

    // The main function always ends with a call to pthred_exit
    if (irs->main) {
        ir_pthread_exit(irs->B);
        LLVMBuildRetVoid(irs->B);
        state_close_block(irs);
        return;
    }

    // Initializers always returns the 'self' reference
    if (irs->initializer) {
        assert(llvm_value);
        LLVMBuildRet(irs->B, llvm_value);
        state_close_block(irs);
        return;
    }

    // Return for normal functions and methods
    if (llvm_value) {
        LLVMBuildRet(irs->B, llvm_value);
    } else {
        LLVMBuildRetVoid(irs->B);
    }
    state_close_block(irs);
}

// TODO: This functions is wrong, should use LLVMConstString and cast to pointer
static LLVMValueRef stringliteral(IRState* irs, const char* string) {
    // Global
    size_t len = strlen(string);
    LLVMValueRef llvm_global = LLVMAddGlobal(
        irs->M,
        LLVMArrayType(LLVMInt8Type(), len + 1),
        LLVM_GLOBAL_STRING
    );
    LLVMSetInitializer(llvm_global, LLVMConstString(string, len, false));
    LLVMSetVisibility(llvm_global, LLVMHiddenVisibility);

    // Expression
    return LLVMBuildPointerCast(
        irs->B,
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
static LLVMValueRef zerovalue(IRState* irs, Type* type) {
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
            return stringliteral(irs, "");
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
