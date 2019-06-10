#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: remove
#include <strings.h> // TODO: remove

#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"
#include "athreads.h"
#include "ir.h"
#include "list.h"
#include "macros.h"
#include "parser.h" // for the tokens

/*
 * TODO
 *  - Create macro for declares?
 *  - Create macro for function calls?
 *  - malloc error: mallocs que vc da na main podem ser perdidos quando a main
 *      termina e vc está usando esse malloc em outra thread?
 *  - free and destroy the mutex one day
 */

typedef LLVMBuilderRef    LLVMB  ;
typedef LLVMModuleRef     LLVMM  ;
typedef LLVMTypeRef       LLVMT  ;
typedef LLVMBasicBlockRef LLVMBB ;
typedef LLVMValueRef      LLVMV  ;

// string malloc that accounts for '\0'
#define SMALLOC(str, len) MALLOC_ARRAY(str, char, (len + 1))

// TODO: move / other names
#define NAME_THREAD_ARGUMENTS_STRUCTURE "_thread_arguments"
#define NAME_SPAWN_FUNCTION             "_spawn_block"

#define STRUCTURE_MUTEX             (0)
#define STRUCTURE_VMT               (1)
#define STRUCTURE_ATTRIBUTE_START   (STRUCTURE_VMT + 1)

#define LABEL                   ""
#define LABEL_ENTRY             LABEL "entry"
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

// primitive types
static Type* __void;
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// auxiliary
static LLVMV stringliteral(IRState*, const char*);
static LLVMV zerovalue(IRState*, Type*);

// TODO: move / rename -> see position_builder
static void state_close_block(IRState* irs) {
    assert(irs->block);
    irs->block = NULL;
}

// LLVM (TODO: new module?)
static LLVMT llvm_type(Type*);
static LLVMT llvm_fn_type(Definition*, size_t);
static void llvm_return(IRState*, LLVMV);
static LLVMT llvm_structure(LLVMT[], size_t, const char*);

// structures' static initializers
List* inits = NULL;

// ==================================================
//
//  Globals
//
// ==================================================

// TODO: move / refactor => use List.h

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

// TODO: move to auxiliary area
// returns the mutex lock from a monitor
static LLVMV monitormutex(LLVMB B, LLVMV monitor) {
    LLVMV x = LLVMBuildLoad(B,
        LLVMBuildStructGEP(B, monitor, STRUCTURE_MUTEX, "mutexPtr"), "mutex"
    ); // TODO: temps
    return x;
}

static void todospawn(IRState*, FunctionCall*);

// ==================================================
//
//  backend.c
//
// ==================================================

static void backend_definition(IRState*, Definition*);
static void backend_block(IRState*, Block*);
static void backend_statement(IRState*, Statement*);
static void backend_capsa(IRState*, Capsa*);
static void backend_expression(IRState*, Expression*);
static void backend_condition(IRState*, Expression*, LLVMBB, LLVMBB);
static LLVMV backend_function_call(IRState*, FunctionCall*);

static IRState* setup(void);
static LLVMM teardown(IRState*);

LLVMM compile(AST* ast) {
    IRState* irs = setup();
    for (Definition* d = ast->definitions; d; d = d->next) {
        backend_definition(irs, d);
    }
    LLVMM M = teardown(irs);
    return M;
}

// -----------------------------------------------------------------------------

static IRState* setup(void) {
    // primitive types
    __void = ast_type_void();
    __boolean = ast_type_boolean();
    __integer = ast_type_integer();
    __float = ast_type_float();
    __string = ast_type_string();
    __condition_queue = ast_type_condition_queue();

    // ir
    IRState* irs = ir_state_new(
        LLVMModuleCreateWithName("main.aria"), LLVMCreateBuilder()
    );
    ir_setup(irs->M);
    ir_pthread_setup(irs->M);

    inits = list_new();

    return irs;
}

static LLVMM teardown(IRState* irs) {
    ir_state_done(irs);
    LLVMM M = irs->M;
    ir_state_free(irs);
    list_destroy(inits);
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

static const char* fname(Definition*, int);
static void fnL(IRState*, Definition*);
static void fnNL(IRState*, Definition*);

#define ISPRIVATE(fn) (fn->function.qualifiers & FQ_PRIVATE)

static LLVMV vmtnew(LLVMB, int);
static void vmtstore(LLVMB, LLVMV, LLVMV, int);
static void vmtL(LLVMB, LLVMV, Definition**, int);
static void vmtNL(LLVMB, LLVMV, Definition**, int);
static void vmtinit(IRState*, Definition*);

static void backend_definition(IRState* irs, Definition* d) {
    switch (d->tag) {
    case DEFINITION_CAPSA:       backend_definition_capsa(irs, d);       break;
    case DEFINITION_FUNCTION:    backend_definition_function(irs, d);    break;
    case DEFINITION_METHOD:      backend_definition_method(irs, d);      break;
    case DEFINITION_CONSTRUCTOR: backend_definition_constructor(irs, d); break;
    case DEFINITION_TYPE:
        switch (d->type->tag) {
        case TYPE_INTERFACE: backend_definition_interface(irs, d); break;
        case TYPE_STRUCTURE: backend_definition_structure(irs, d); break;
        case TYPE_MONITOR:   backend_definition_monitor(irs, d);   break;
        default:             UNREACHABLE;
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
        capsa->V = exp->V;
    } else { // variables
        capsa->V = LLVMBuildAlloca(
            irs->B, llvm_type(capsa->type), capsa->id->name
        );
        if (exp) {
            backend_expression(irs, exp);
            LLVMBuildStore(
                irs->B, exp->V, capsa->V
            );
        }
    }
}

static void backend_definition_function(IRState* irs, Definition* def) {
    initializefunction(irs, def);

    if (def->function.id->name == scanner_native[SCANNER_NATIVE_MAIN]) {
        irs->main = true;
        initializeglobals(irs);
        // calling all structures' initializers
        FOREACH(ListNode, n, inits->first) {
            LLVMBuildCall(irs->B, (LLVMV)n->value, NULL, 0, LLVM_TMP_NONE);
        }
    }

    backend_block(irs, def->function.block);

    // implicit return
    if (irs->block) {
        llvm_return(irs, zerovalue(irs, def->function.type));
    }

    irs->main = false;
}

static void backend_definition_method(IRState* irs, Definition* def) {
    fnNL(irs, def);
    fnL(irs, def);
}

// TODO: move
static LLVMV generic_pointer(LLVMB B, LLVMV ptr) {
    return LLVMBuildBitCast(B, ptr, LLVMT_PTR_VOID, LLVM_TMP);
}

static void backend_definition_constructor(IRState* irs, Definition* def) {
    irs->initializer = true;

    initializefunction(irs, def);

    // creates the <self> instance
    irs->self = LLVMBuildMalloc(irs->B, irs->structure_type, LLVM_TMP);

    // creates the monitor's mutex
    LLVMV mutex = ir_malloc(irs->B, sizeof(pthread_mutex_t));
    ir_pthread_mutex_init(irs->B, mutex);
    LLVMBuildStore(irs->B, mutex, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCTURE_MUTEX, LLVM_TMP
    ));

    // stores the VMT-L inside the <self> instance
    LLVMV gL = def->function.type->structure.gL;
    assert(gL);
    LLVMV vmt = LLVMBuildLoad(irs->B, gL, LLVM_TMP);
    LLVMBuildStore(irs->B, vmt, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCTURE_VMT, LLVM_TMP_VMT
    ));

    // initializes attributes
    FOREACH(Definition, d, def->function.type->structure.definitions) {
        if (d->tag != DEFINITION_CAPSA) {
            continue;
        }
        backend_capsa(irs, d->capsa.capsa);
        LLVMV expression;
        if (d->capsa.expression) {
            backend_expression(irs, d->capsa.expression);
            expression = d->capsa.expression->V;
        } else {
            expression = LLVMGetUndef(llvm_type(d->capsa.capsa->type));
        }
        LLVMBuildStore(irs->B, expression, d->capsa.capsa->V);
    }

    backend_block(irs, def->function.block);

    // TODO: why is this IF here? should check the tests
    if (irs->block) {
        llvm_return(irs, generic_pointer(irs->B, irs->self));
    }

    irs->self = NULL;
    irs->initializer = false;
}

static void backend_definition_interface(IRState* irs, Definition* def) {
    size_t vmt_size = def->type->structure.methods_size;
    LLVMT fields[] = {
        LLVM_TYPE_POINTER_PTHREAD_MUTEX_T,
        LLVMT_PTR(LLVMArrayType(LLVMT_PTR_VOID, vmt_size))
    };
    const char* name = def->type->structure.id->name;
    def->type->T = llvm_structure(fields, 2, name);
}

static void backend_definition_structure(IRState* irs, Definition* def) {
    UNREACHABLE; // TODO
}

static void backend_definition_monitor(IRState* irs, Definition* def) {
    size_t attributes_size = def->type->structure.attributes_size;
    LLVMT attributes[STRUCTURE_ATTRIBUTE_START + attributes_size];

    // mutex & VMT
    attributes[STRUCTURE_MUTEX] = LLVM_TYPE_POINTER_PTHREAD_MUTEX_T;
    attributes[STRUCTURE_VMT] = LLVMT_PTR(LLVMArrayType(
        LLVMT_PTR_VOID, def->type->structure.methods_size
    ));

    // attributes
    for (int i = 0; i < def->type->structure.attributes_size; i++) {
        Capsa* capsa = def->type->structure.attributes[i]->capsa.capsa;
        capsa->llvm_structure_index = STRUCTURE_ATTRIBUTE_START + i;
        attributes[STRUCTURE_ATTRIBUTE_START + i] = llvm_type(capsa->type);
    }

    def->type->T = llvm_structure(
        attributes,
        def->type->structure.attributes_size + STRUCTURE_ATTRIBUTE_START,
        def->type->structure.id->name
    );

    // methods
    irs->structure_type = def->type->T;
    for (int i = 0; i < def->type->structure.methods_size; i++) {
        Definition* fn = def->type->structure.methods[i]; // FIXME: ?
        backend_definition(irs, fn);
    }

    // VMT
    vmtinit(irs, def);

    // constructor
    backend_definition(irs, def->type->structure.constructor);
    irs->structure_type = NULL;
}

// -----------------------------------------------------------------------------

// auxiliary
// declares a function in LLVM
// links parameters' LLVM values inside the AST
// creates the ENTRY blocks and positions the instruction builder
// defines (fn->V) and (irs->function)
static void initializefunction(IRState* irs, Definition* fn) {
    assert(fn->function.id);
    assert(fn->function.type);

    // counts the number of parameters
    int params = 0;
    FOREACH(Definition, p, fn->function.parameters) {
        params++;
    }

    // creates the function prototype 
    fn->V = LLVMAddFunction(irs->M,
        fn->function.id->name, llvm_fn_type(fn, params)
    );

    { // assigns LLVM values to each parameter
        int i = 0;
        FOREACH(Definition, p, fn->function.parameters) {
            p->capsa.capsa->V = LLVMGetParam(fn->V, i++);
        }
    }

    position_builder(irs, LLVMAppendBasicBlock(fn->V, LABEL_ENTRY));

    irs->function = fn->V;
}

// auxiliary - initializes all global values
static void initializeglobals(IRState* irs) {
    FOREACH(Global, global, globals) {
        Capsa* capsa = global->value->capsa.capsa;
        Expression* expression = global->value->capsa.expression;
        LLVMT T = llvm_type(capsa->type);
        capsa->V = LLVMAddGlobal(irs->M, T, capsa->id->name);
        LLVMSetInitializer(capsa->V, LLVMGetUndef(T));
        backend_expression(irs, expression);
        LLVMBuildStore(irs->B, expression->V, capsa->V);
    }
}

// nll = [ 0 => non-locking ; 1 => locking ]
static const char* fname(Definition* fn, int nll) {
    char* s;
    int len;
    if (fn->function.qualifiers & FQ_ACQUIRE) {
        len = strlen(fn->function.id->name) + 8;
        SMALLOC(s, len);
        strcpy(s, "acquire-");
    } else if (fn->function.qualifiers & FQ_RELEASE) {
        len = strlen(fn->function.id->name) + 8;
        SMALLOC(s, len);
        strcpy(s, "release-");
    } else {
        len = strlen(fn->function.id->name);
        SMALLOC(s, len);
        s[0] = '\0';
    }
    strcat(s, fn->function.id->name);

    char* name;
    switch (nll) {
    case 0: // non-locking
        SMALLOC(name, len + 3);
        strcpy(name, s);
        strcat(name, "-NL");
        break;
    case 1: // locking
        SMALLOC(name, len + 2);
        strcpy(name, s);
        strcat(name, "-L");
        break;
    default:
        UNREACHABLE;
    }
    free(s);

    return name;
}

// TODO: doc
static void fnNL(IRState* irs, Definition* fn) {
    // counts the number of parameters
    int n = 0;
    COUNT(Definition, fn->function.parameters, n);

    // adds the non-locking-function's prototype to the module
    const char* name = fname(fn, 0);
    fn->V = LLVMAddFunction(irs->M, name, llvm_fn_type(fn, n));
    free((void*)name);

    // assigns LLVM values to each parameter
    n = 0;
    FOREACH(Definition, p, fn->function.parameters) {
        p->capsa.capsa->V = LLVMGetParam(fn->V, n++);
    }

    position_builder(irs, LLVMAppendBasicBlock(fn->V, LABEL_ENTRY));
    irs->function = fn->V;

    // <self> is the first parameter
    LLVMV selfV = fn->function.parameters->capsa.capsa->V;
    LLVMT selfT = LLVMT_PTR(irs->structure_type);
    irs->self = LLVMBuildBitCast(irs->B, selfV, selfT, LLVM_TMP_SELF);

    backend_block(irs, fn->function.block);

    // implicit return
    if (irs->block) {
        llvm_return(irs, zerovalue(irs, fn->function.type));
    }

    irs->self = NULL;
}

// TODO: doc
static void fnL(IRState* irs, Definition* fn) {
    assert(fn->V);

    // adds the locking-function's prototype to the module
    int argc = LLVMCountParams(fn->V);
    const char* name = fname(fn, 1);
    LLVMV fnL = LLVMAddFunction(irs->M, name, llvm_fn_type(fn, argc));
    free((void*)name);

    position_builder(irs, LLVMAppendBasicBlock(fnL, LABEL_ENTRY));

    // gets the monitor's mutex from <self>
    LLVMV selfV = LLVMGetParam(fnL, 0);
    LLVMT selfT = LLVMT_PTR(irs->structure_type);
    selfV = LLVMBuildBitCast(irs->B, selfV, selfT, LLVM_TMP_SELF);
    LLVMV mutex = monitormutex(irs->B, selfV);

    ir_pthread_mutex_lock(irs->B, mutex);

    // TODO: when calling the NL function, should pass the M! object
    //       with the unlocked VMT

    // calls the NL function
    LLVMV argv[argc];
    LLVMGetParams(fnL, argv);
    LLVMV ret = (fn->function.type != __void)
        ? LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_RETURN)
        : (LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_NONE), NULL)
        ;

    ir_pthread_mutex_unlock(irs->B, mutex);

    llvm_return(irs, (ret) ? ret : zerovalue(irs, fn->function.type));

    fn->lV = fnL;
}

// -----------------------------------------------------------------------------

// allocates memory for the VMT
static LLVMV vmtnew(LLVMB B, int n) {
    LLVMT T = LLVMArrayType(LLVMT_PTR_VOID, n);
    LLVMV V = LLVM_CONST_INT(n);
    V = LLVMBuildArrayMalloc(B, LLVMT_PTR_VOID, V , LLVM_TMP_VMT);
    return LLVMBuildBitCast(B, V, LLVMT_PTR(T), LLVM_TMP_VMT);
}

// stores a function in a give position of the VMT
static void vmtstore(LLVMB B, LLVMV vmt, LLVMV fn, int n) {
    assert(vmt && fn);
    LLVMV indices[] = {LLVM_CONST_INT(0), LLVM_CONST_INT(n)};
    LLVMV P = LLVMBuildGEP(B, vmt, indices, 2, LLVM_TMP);
    LLVMV V = LLVMBuildBitCast(B, fn, LLVMT_PTR_VOID, LLVM_TMP);
    LLVMBuildStore(B, V, P);
}

// fills a locking VMT with methods
static void vmtL(LLVMB B, LLVMV vmt, Definition** methods, int n) {
    int j = 0;
    for (int i = 0; i < n; i++) {
        if (ISPRIVATE(methods[i])) { continue; }
        vmtstore(B, vmt, methods[i]->lV, j);
        methods[i]->function.vmt_index = j++;
        // TODO: vmt_index should only be set in vmtNL
    }
}

// fills a non-locking VMT with methods
static void vmtNL(LLVMB B, LLVMV vmt, Definition** methods, int n) {
    int j = 0;
    for (int i = 0; i < n; i++) {
        if (ISPRIVATE(methods[i])) { continue; }
        vmtstore(B, vmt, methods[i]->V, j);
        methods[i]->function.vmt_index = j++;
    }
    for (int i = 0; i < n; i++) {
        if (!ISPRIVATE(methods[i])) { continue; }
        vmtstore(B, vmt, methods[i]->V, j);
        methods[i]->function.vmt_index = j++;
    }
}

// TODO: move
static char* concat(char* a, char* b) {
    char* s;
    SMALLOC(s, strlen(a) + strlen(b));
    strcpy(s, a); 
    strcat(s, b);
    return s;
}

// auxiliary - creates the structure's static initializer and its global VMT(s)
static void vmtinit(IRState* irs, Definition* m) {
    Definition** methods = m->type->structure.methods;
    int n = m->type->structure.methods_size;
    // TODO: n only for NL, count privates

    // creates the static constructor function for the monitor
    LLVMT T = LLVMFunctionType(LLVMVoidType(), NULL, 0, false);
    LLVMV init = LLVMAddFunction(irs->M, "static-constructor", T);
    LLVMPositionBuilderAtEnd(irs->B, LLVMAppendBasicBlock(init, LABEL_ENTRY));

    // creates and fills the VMT with the monitor's methods
    LLVMV L = vmtnew(irs->B, n);
    vmtL(irs->B, L, methods, n);
    LLVMV NL = vmtnew(irs->B, n);
    vmtNL(irs->B, NL, methods, n);

    // TODO: change VMTs to remove malloc and set global
    //       type as [N x i8*] instead of [N x i8*]*

    // stores the VMTs in global variables
    T = LLVMT_PTR(LLVMArrayType(LLVMT_PTR_VOID, n));

    char* s;

    s = concat((char*)m->type->structure.id->name, "-vmt-L");
    LLVMV gL = LLVMAddGlobal(irs->M, T, s);
    free(s);
    LLVMSetInitializer(gL, LLVMGetUndef(T));
    LLVMBuildStore(irs->B, L, gL);

    s = concat((char*)m->type->structure.id->name, "-vmt-NL");
    LLVMV gNL = LLVMAddGlobal(irs->M, T, s);
    free(s);
    LLVMSetInitializer(gNL, LLVMGetUndef(T));
    LLVMBuildStore(irs->B, NL, gNL);

    LLVMBuildRetVoid(irs->B);

    m->type->structure.gL = gL;
    m->type->structure.gNL = gNL;
    list_append(inits, (ListValue) init);
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
//  Statement
//
// ==================================================

/*
static void backend_statement(IRState* irs, Statement* stmt) {
    switch (stmt->tag) {
    case STATEMENT_ASSIGNMENT:
        UNREACHABLE;
        break;
    case STATEMENT_FUNCTION_CALL:
        UNREACHABLE;
        break;
    case STATEMENT_WAIT_FOR_IN:
        UNREACHABLE;
        break;
    case STATEMENT_SIGNAL:
        UNREACHABLE;
        break;
    case STATEMENT_BROADCAST:
        UNREACHABLE;
        break;
    case STATEMENT_RETURN:
        UNREACHABLE;
        break;
    case STATEMENT_IF:
        UNREACHABLE;
        break;
    case STATEMENT_IF_ELSE:
        UNREACHABLE;
        break;
    case STATEMENT_WHILE:
        UNREACHABLE;
        break;
    case STATEMENT_FOR:
        UNREACHABLE;
        break;
    case STATEMENT_SPAWN:
        UNREACHABLE;
        break;
    case STATEMENT_ACQUIRE_VALUE:
        UNREACHABLE;
        break;
    case STATEMENT_BLOCK:
        UNREACHABLE;
        break;
    default:
        UNREACHABLE;
    }
}
*/

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
            statement->assignment.expression->V,
            statement->assignment.capsa->V
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
        LLVMV
            mutex = monitormutex(irs->B, irs->self),
            indices[1] = {LLVM_CONST_INT(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->wait_for_in.queue->V,
                indices,
                1,
                LLVM_TMP
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
        LLVMV
            indices[1] = {LLVM_CONST_INT(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->signal->V,
                indices,
                1,
                LLVM_TMP
            )
        ;
        ir_pthread_cond_signal(irs->B, cond);
        break;
    }
    case STATEMENT_BROADCAST:
        backend_expression(irs, statement->broadcast);
        LLVMV
            indices[1] = {LLVM_CONST_INT(0)},
            cond = LLVMBuildGEP(
                irs->B,
                statement->broadcast->V,
                indices,
                1,
                LLVM_TMP
            )
        ;
        ir_pthread_cond_broadcast(irs->B, cond);
        break;
    case STATEMENT_RETURN:
        if (statement->return_) {
            backend_expression(irs, statement->return_);
            llvm_return(irs, statement->return_->V);
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
        // V dealt with already, unless...
        if (capsa->llvm_structure_index > -1) { // attributes
            assert(irs->self);
            capsa->V = LLVMBuildStructGEP(
                irs->B, irs->self, capsa->llvm_structure_index, LLVM_TMP
            );
        }
        break;
    case CAPSA_INDEXED: {
        backend_expression(irs, capsa->indexed.array);
        backend_expression(irs, capsa->indexed.index);
        LLVMV indices[1] = {capsa->indexed.index->V};
        capsa->V = LLVMBuildGEP(
            irs->B,
            capsa->indexed.array->V,
            indices,
            1,
            LLVM_TMP
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
        expression->V =
            LLVM_CONSTANT_BOOLEAN(expression->literal.boolean);
        break;
    case EXPRESSION_LITERAL_INTEGER:
        expression->V =
            LLVM_CONST_INT(expression->literal.integer);
        break;
    case EXPRESSION_LITERAL_FLOAT:
        expression->V = LLVM_CONSTANT_FLOAT(expression->literal.float_);
        break;
    case EXPRESSION_LITERAL_STRING:
        expression->V = stringliteral(
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
        LLVMV size = LLVM_CONST_INT(n);
        expression->V = LLVMBuildArrayMalloc(
            irs->B,
            llvm_type(expression->type->array),
            size,
            LLVM_TMP
        );
        // setting the values inside the array
        n = 0;
        for (Expression* e = expression->literal.array; e; e = e->next) {
            LLVMV
                indices[1] = {LLVM_CONST_INT(n++)},
                pointer = LLVMBuildGEP(
                    irs->B,
                    expression->V,
                    indices,
                    1,
                    LLVM_TMP
                )
            ;
            LLVMBuildStore(irs->B, e->V, pointer);
        }
        break;
    }
    case EXPRESSION_CAPSA:
        backend_capsa(irs, expression->capsa);
        // TODO: Find better way to write this
        if (expression->capsa->llvm_structure_index > -1) { // attributes
            expression->V = LLVMBuildLoad(
                irs->B,
                expression->capsa->V,
                LLVM_TMP
            );
        } else if (expression->capsa->value && !expression->capsa->global) {
            // local values
            expression->V = expression->capsa->V;
        } else { // variables
            expression->V = LLVMBuildLoad(
                irs->B,
                expression->capsa->V,
                LLVM_TMP
            );
        }
        break;
    case EXPRESSION_FUNCTION_CALL:
        expression->V = backend_function_call(
            irs, expression->function_call
        );
        break;
    case EXPRESSION_UNARY:
        switch (expression->unary.token) {
        case '-':
            backend_expression(irs, expression->unary.expression);
            if (expression->type == __integer) {
                expression->V = LLVMBuildNeg(irs->B,
                    expression->unary.expression->V, LLVM_TMP);
            } else if (expression->type == __float) {
                expression->V = LLVMBuildFNeg(irs->B,
                    expression->unary.expression->V, LLVM_TMP);
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
                e->V = ifunc(s->B, \
                    e->binary.left_expression->V, \
                    e->binary.right_expression->V, LLVM_TMP); \
            } else if (e->type == __float) { \
                e->V = ffunc(s->B, \
                    e->binary.left_expression->V, \
                    e->binary.right_expression->V, LLVM_TMP); \
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
            expression->V = LLVMBuildSIToFP(
                irs->B,
                expression->cast->V,
                llvm_type(expression->type),
                LLVM_TMP
            );
        } else if (from == __float && to == __integer) {
            // Float to Integer
            expression->V = LLVMBuildFPToSI(
                irs->B,
                expression->cast->V,
                llvm_type(expression->type),
                LLVM_TMP
            );
        } else if (from->tag == TYPE_MONITOR && to->tag == TYPE_INTERFACE) {
            // Monitor to Interface
            expression->V = expression->cast->V;
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

        LLVMV
            phi = LLVMBuildPhi(
                irs->B, LLVM_TYPE_BOOLEAN, LLVM_TEMPORARY_PHI
            ),
            incoming_values[2] = {LLVM_CONSTANT_TRUE, LLVM_CONSTANT_FALSE}
        ;
        LLVMBasicBlockRef incoming_blocks[2] = {block_true, block_false};
        LLVMAddIncoming(phi, incoming_values, incoming_blocks, 2);

        expression->V = phi;
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
            expression->V = ir_cmp(
                irs->B, LLVMIntEQ, LLVMRealOEQ, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
            state_close_block(irs);
            break;
        case TK_NEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->V = ir_cmp(
                irs->B, LLVMIntNE, LLVMRealONE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
            state_close_block(irs);
            break;
        case TK_LEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->V = ir_cmp(
                irs->B, LLVMIntSLE, LLVMRealOLE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
            state_close_block(irs);
            break;
        case TK_GEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->V = ir_cmp(
                irs->B, LLVMIntSGE, LLVMRealOGE, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
            state_close_block(irs);
            break;
        case '<':
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->V = ir_cmp(
                irs->B, LLVMIntSLT, LLVMRealOLT, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
            state_close_block(irs);
            break;
        case '>':
            backend_expression(irs, l);
            backend_expression(irs, r);
            expression->V = ir_cmp(
                irs->B, LLVMIntSGT, LLVMRealOGT, l, r
            );
            LLVMBuildCondBr(irs->B, expression->V, lt, lf);
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
        LLVMBuildCondBr(irs->B, expression->V, lt, lf);
        state_close_block(irs);
    }
}

// ==================================================
//
//  FunctionCall
//
// ==================================================

static LLVMV backend_fc_basic(IRState*, FunctionCall*);
static LLVMV backend_fc_method(IRState*, FunctionCall*);
static LLVMV backend_fc_condition_queue(IRState*, FunctionCall*);
static LLVMV backend_fc_array(IRState*, FunctionCall*);

static LLVMV* fcargs(IRState*, FunctionCall*);
static LLVMV nativefc(IRState*, FunctionCall*);

static LLVMV ir_array_get(LLVMB, LLVMV, int);
static LLVMV getvmt(LLVMB, LLVMV);

static LLVMV backend_function_call(IRState* irs, FunctionCall* fc) {
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

static LLVMV backend_fc_basic(IRState* irs, FunctionCall* fc) {
    LLVMV V;
    if ((V = nativefc(irs, fc))) {
        return V;
    }
    LLVMV* args = fcargs(irs, fc);
    V = LLVMBuildCall(irs->B, fc->fn->V, args, fc->argc, LLVM_TMP_NONE);
    free(args);
    return V;
}

static LLVMV backend_fc_method(IRState* irs, FunctionCall* fc) {
    LLVMV* args = fcargs(irs, fc);
    LLVMV obj = LLVMBuildBitCast(irs->B,
        args[0], LLVMT_PTR(fc->obj->type->T), LLVM_TMP
    );

    // bitcast from i8* to (T)*
    // FIXME: LLVMT T = llvm_fn_type(fc->fn, fc->argc);
    LLVMT T; // function type
    {
        int i = 1;
        LLVMT params[fc->argc]; // parameter types
        params[0] = LLVMT_AOBJ;
        FOREACH(Definition, p, fc->fn->function.parameters->next) {
            params[i++] = llvm_type(p->capsa.capsa->type);
        }
        T = llvm_type(fc->fn->function.type); // return type
        T = LLVMFunctionType(T, params, fc->argc, false);
    }

    int vmti = fc->fn->function.vmt_index;
    if (fc->obj->type->tag == TYPE_INTERFACE) { // FIXME: gambiarra
        vmti += 2; // accounting for the <unlocked> pair
    }

    // VMT function call
    LLVMV vmt = getvmt(irs->B, obj);
    LLVMV fn = ir_array_get(irs->B, vmt, vmti);
    fn = LLVMBuildBitCast(irs->B, fn, LLVMT_PTR(T), LLVM_TMP);
    LLVMV V = LLVMBuildCall(irs->B, fn, args, fc->argc, LLVM_TMP_NONE);

    free(args);
    return V;
}

static LLVMV backend_fc_condition_queue(IRState* irs, FunctionCall* fc) {
    assert(fc->type == __condition_queue);
    LLVMV V = ir_malloc(irs->B, sizeof(pthread_cond_t));
    ir_pthread_cond_init(irs->B, V);
    return V;
}

static LLVMV backend_fc_array(IRState* irs, FunctionCall* fc) {
    assert(fc->argc == 1);
    Expression* size = fc->arguments;
    backend_expression(irs, size);
    return LLVMBuildArrayMalloc(
        irs->B,
        llvm_type(fc->type->array),
        size->V,
        LLVM_TMP
    );
}

// -----------------------------------------------------------------------------

// auxiliary
// returns an array with the function call's arguments
// the returned array must be freed
static LLVMV* fcargs(IRState* irs, FunctionCall* fc) {
    assert(fc->argc >= 0);
    FOREACH(Expression, e, fc->arguments) {
        backend_expression(irs, e);
    }
    LLVMV* args;
    MALLOC_ARRAY(args, LLVMV, fc->argc);
    int i = 0;
    FOREACH(Expression, e, fc->arguments) {
        args[i++] = e->V;
    }
    return args;
}

// auxiliary - provides the implementation for calls to native functions
static LLVMV nativefc(IRState* irs, FunctionCall* fc) {
    if (fc->fn) {
        return NULL;
    }
    LLVMV V = NULL;
    LLVMV* args = fcargs(irs, fc);
    if (!strcmp(fc->id->name, "print")) {
        V = ir_printf(irs->B, args, fc->argc);
    } else {
        UNREACHABLE;
    }
    free(args);
    return V;
}

// TODO: doc
static LLVMV getvmt(LLVMB B, LLVMV obj) {
    obj = LLVMBuildStructGEP(B, obj, STRUCTURE_VMT, LLVM_TMP);
    return LLVMBuildLoad(B, obj, LLVM_TMP_VMT);
}

// returns (*ptr)[i], given <ptr> is a pointer to an array
static LLVMV ir_array_get(LLVMB B, LLVMV ptr, int i) {
    LLVMV indices[] = {LLVM_CONST_INT(0), LLVM_CONST_INT(i)};
    ptr = LLVMBuildGEP(B, ptr, indices, 2, LLVM_TMP);
    return LLVMBuildLoad(B, ptr, LLVM_TMP);
}

// ==================================================
//
//  LLVM
//
// ==================================================

static LLVMT llvm_type(Type* type) {
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
static LLVMT llvm_fn_type(Definition* fn, size_t psize) {
    int i = 0;
    LLVMT params[psize];
    FOREACH(Definition, p, fn->function.parameters) {
        params[i++] = llvm_type(p->capsa.capsa->type);
    }
    LLVMT T = llvm_type(fn->function.type);
    return LLVMFunctionType(T, params, psize, false);
}

// TODO: Rename this: backend_return?
static void llvm_return(IRState* irs, LLVMV V) {
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
        assert(V);
        LLVMBuildRet(irs->B, V);
        state_close_block(irs);
        return;
    }

    // Return for normal functions and methods
    if (V) {
        LLVMBuildRet(irs->B, V);
    } else {
        LLVMBuildRetVoid(irs->B);
    }
    state_close_block(irs);
}

// TODO: This functions is wrong, should use LLVMConstString and cast to pointer
static LLVMV stringliteral(IRState* irs, const char* string) {
    // Global
    size_t len = strlen(string);
    LLVMV llvm_global = LLVMAddGlobal(irs->M,
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
        LLVM_TMP
    );
}

// TODO: doc
static LLVMT llvm_structure(LLVMT fields[], size_t size,
    const char* name) {

    LLVMT type = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
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
static LLVMV zerovalue(IRState* irs, Type* type) {
    switch (type->tag) {
    case TYPE_VOID:
        return NULL;
    case TYPE_ID:
        if (type == __boolean) {
            return LLVM_CONSTANT_FALSE;
        }
        if (type == __integer) {
            return LLVM_CONST_INT(0);
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

// ==================================================
//
//  TODO
//
// ==================================================

// TODO: rename / fix
static void todospawn(IRState* irs, FunctionCall* call) {
    // Defining the structure to be used for argument passing
    LLVMT fields[call->argc];
    unsigned int n = 0;
    for (Expression* e = call->arguments; e; e = e->next, n++) {
        fields[n] = llvm_type(e->type);
    }
    LLVMT type_structure = llvm_structure(
        fields, n, NAME_THREAD_ARGUMENTS_STRUCTURE
    );

    // Allocating memory for the structure
    LLVMV structure = LLVMBuildMalloc(
        irs->B, type_structure, LLVM_TMP
    );

    // Filling the structure with values from the arguments
    n = 0;
    for (Expression* e = call->arguments; e; e = e->next, n++) {
        backend_expression(irs, e);
        LLVMBuildStore(irs->B, e->V, LLVMBuildStructGEP(
            irs->B, structure, n, LLVM_TMP_NONE
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
    LLVMV parameter = LLVMBuildBitCast(
        spawn_irs->B,
        LLVMGetParam(spawn_irs->function, 0),
        LLVMT_PTR(type_structure),
        LLVM_TMP
    );
    Definition* p = call->fn->function.parameters;
    for (unsigned int n = 0; p; p = p->next, n++) {
        p->capsa.capsa->V = LLVMBuildLoad(
            spawn_irs->B,
            LLVMBuildStructGEP(
                spawn_irs->B, parameter, n, LLVM_TMP
            ),
            LLVM_TMP
        );
    }

    // Block
    backend_block(spawn_irs, call->fn->function.block);

    // TODO: Should free, but weird error (not this...)
    // LLVMBuildFree(irs->B, parameter);
    LLVMBuildRet(
        spawn_irs->B, LLVMConstPointerNull(LLVMT_PTR_VOID)
    );

    // Going back to the original irs builder position
    LLVMPositionBuilderAtEnd(irs->B, irs->block);

    // Calling pthread_create
    ir_pthread_create(
        irs->B,
        spawn_irs->function,
            LLVMBuildBitCast(
            irs->B, structure, LLVMT_PTR_VOID, LLVM_TMP
        )
    );

    ir_state_free(spawn_irs);
}
