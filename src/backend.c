#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h> // TODO: remove

#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"
#include "ir.h"
#include "list.h"
#include "macros.h"
#include "parser.h" // for the tokens

/*
 * TODO
 *  - free and destroy the mutex one day
 */

// string malloc that accounts for '\0'
#define SMALLOC(str, len) MALLOC_ARRAY(str, char, (len + 1))

#define STRUCT_IDX_MUTEX    (0)
#define STRUCT_IDX_VMT      (1)
#define STRUCT_IDX_FIELD0   (STRUCT_IDX_VMT + 1)

#define PROXY_IDX_OBJ   (0)
#define PROXY_IDX_VMTP  (STRUCT_IDX_VMT)
#define PROXY_IDX_OK    (2)

// basic block names
#define BB_ENTRY ("entry")
#define BB_IF    ("if")
#define BB_ELSE  ("else")
#define BB_COND  ("cond")
#define BB_LOOP  ("loop")
#define BB_INC   ("inc")
#define BB_END   ("end")

// primitive types
static Type* __void;
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// concatanates two strings (the returned string must be freed)
static char* concat(char* a, char* b) {
    char* s;
    SMALLOC(s, strlen(a) + strlen(b));
    strcpy(s, a);
    strcat(s, b);
    return s;
}

// auxiliary
static LLVMT llvmtype(Type*);
static LLVMV llvmzero(Type*);
static LLVMT llvmstruct(LLVMT[], size_t, const char*);
static LLVMT llvm_fn_type(Definition*, size_t);

// TODO : move
static LLVMV getvmt(LLVMB, LLVMV);
static LLVMV ir_array_get(LLVMB, LLVMV, int);

static List* inits = NULL;   // structures' static initializers
static List* globals = NULL; // global values

// ==================================================
//
//  TODO: Misc
//
// ==================================================

// TODO: move to auxiliary area
// auxiliary - returns the mutex lock from a monitor object
static LLVMV monitormutex(LLVMB B, LLVMV obj) {
    LLVMV V = LLVMBuildStructGEP(B, obj, STRUCT_IDX_MUTEX, "mutex-ptr");
    return LLVMBuildLoad(B, V, "mutex");
}

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
    FOREACH(Definition, d, ast->definitions) {
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
    LLVMM M = LLVMModuleCreateWithName("main.aria");
    LLVMB B = LLVMCreateBuilder();
    IRState* irs = irs_new(M, B);
    ir_setup(M);
    ir_init(B);

    inits = list_new();
    globals = list_new();

    return irs;
}

static LLVMM teardown(IRState* irs) {
    irs_done(irs);
    LLVMM M = irs->M;
    irs_destroy(irs);
    list_destroy(inits);
    list_destroy(globals);
    return M;
}

// ==================================================
//
//  Definition
//
// ==================================================

static void backend_definition_capsa(IRState*, Definition*);
static void backend_definition_function(IRState*, Definition*);
static void backend_definition_constructor(IRState*, Definition*);
static void backend_definition_interface(IRState*, Definition*);
static void backend_definition_structure(IRState*, Definition*);
static void backend_definition_monitor(IRState*, Definition*);

static void initializefunction(IRState*, Definition*);
static void addglobal(IRState*, Definition*);
static void initializeglobals(IRState*);

static const char* fname(Definition*, int);
static LLVMV fnL(IRState*, Definition*, LLVMT);
static LLVMV fnR(IRState*, Definition*, LLVMT);
static LLVMV fnP(IRState*, Definition*, LLVMT);

#define ISPRIVATE(fn) (fn->function.qualifiers & FQ_PRIVATE)

static LLVMV vmtadd(LLVMM, Definition*, char*);
static void vmtset(LLVMB, LLVMV, LLVMV, int);

static LLVMT proxytype(const char*, int);

static void backend_definition(IRState* irs, Definition* d) {
    switch (d->tag) {
    case DEFINITION_CAPSA:       backend_definition_capsa(irs, d);       break;
    case DEFINITION_FUNCTION:    backend_definition_function(irs, d);    break;
    case DEFINITION_CONSTRUCTOR: backend_definition_constructor(irs, d); break;
    case DEFINITION_TYPE:
        switch (d->type->tag) {
        case TYPE_INTERFACE: backend_definition_interface(irs, d); break;
        case TYPE_STRUCTURE: backend_definition_structure(irs, d); break;
        case TYPE_MONITOR:   backend_definition_monitor(irs, d);   break;
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
        addglobal(irs, def);
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
        LLVMT T = llvmtype(capsa->type);
        capsa->V = LLVMBuildAlloca(irs->B, T, capsa->id->name);
        if (exp) {
            backend_expression(irs, exp);
            LLVMBuildStore(irs->B, exp->V, capsa->V);
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

    // checks if it's necessary to place an implicit return instruction
    if (irs->block) {
        irs_return(irs, llvmzero(def->function.type));
    }

    irs->main = false;
}

static void backend_definition_constructor(IRState* irs, Definition* def) {
    irs->initializer = true;

    initializefunction(irs, def);

    // creates the <self> instance
    irs->self = LLVMBuildMalloc(irs->B, irs->structure->type->T, LLVM_TMP);

    // creates the monitor's mutex
    LLVMV mutex = ir_malloc(irs->B, sizeof(pthread_mutex_t));
    irPT_mutex_init(irs->B, mutex);
    LLVMBuildStore(irs->B, mutex, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCT_IDX_MUTEX, LLVM_TMP
    ));

    // stores the VMT-L inside the <self> instance
    assert(def->function.type->structure.gL);
    LLVMBuildStore(irs->B, def->function.type->structure.gL, LLVMBuildStructGEP(
        irs->B, irs->self, STRUCT_IDX_VMT, LLVM_TMP_VMT
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
            expression = LLVMGetUndef(llvmtype(d->capsa.capsa->type));
        }
        LLVMBuildStore(irs->B, expression, d->capsa.capsa->V);
    }

    backend_block(irs, def->function.block);

    // checks if it's necessary to place an implicit return instruction
    if (irs->block) {
        irs_return(irs, NULL);
    }

    irs->self = NULL;
    irs->initializer = false;
}

static void backend_definition_interface(IRState* irs, Definition* def) {
    size_t vmt_size = def->type->structure.methods_size;
    LLVMT fields[] = {
        irPTT_mutex,
        irT_ptr(LLVMArrayType(irT_pvoid, vmt_size))
    };
    const char* name = def->type->structure.id->name;
    def->type->T = llvmstruct(fields, 2, name);
}

static void backend_definition_structure(IRState* irs, Definition* def) {
    UNREACHABLE; // TODO
}

// TODO: function or not?
// auxiliary - returns the type for proxy structures
// type { self: i8*, vmt-P: [4 x i8*]*, ok: i1 }
static LLVMT proxytype(const char* name, int n) {
    char* s = concat((char*)name, "-P");
    LLVMT T = LLVMStructCreateNamed(LLVMGetGlobalContext(), s);
    free(s);
    LLVMT vmt = irT_ptr(LLVMArrayType(irT_pvoid, n));
    LLVMT fields[] = {irT_pvoid, vmt, irT_bool};
    LLVMStructSetBody(T, fields, 3, false);
    return T;
}

static void backend_definition_monitor(IRState* irs, Definition* m) {
    { // llvm structure type
        int fields_sz = m->type->structure.attributes_size;
        int struct_sz = STRUCT_IDX_FIELD0 + fields_sz;
        LLVMT structT[struct_sz];

        // mutex & VMT
        structT[STRUCT_IDX_MUTEX] = irPTT_mutex;
        structT[STRUCT_IDX_VMT] = irT_ptr(LLVMArrayType(
            irT_pvoid, m->type->structure.methods_size
        ));

        // fields
        for (int i = 0; i < fields_sz; i++) {
            Capsa* capsa = m->type->structure.attributes[i]->capsa.capsa;
            capsa->llvm_structure_index = STRUCT_IDX_FIELD0 + i;
            structT[capsa->llvm_structure_index] = llvmtype(capsa->type);
        }

        m->type->T = llvmstruct(structT, struct_sz,m->type->structure.id->name);
    }

    // backend_definition_method (R, L & P)
    // VMT(s)
    // backend_constructor
    Definition** methods = m->type->structure.methods;

    int all = m->type->structure.methods_size; // all functions
    int public = 0; // only public functions
    for (int i = 0; i < all; i++) {
        if (ISPRIVATE(methods[i])) { continue; }
        public++;
    }

    LLVMBB bb;
    { // adds the template initializer function to the module
        LLVMT T = LLVMFunctionType(LLVMVoidType(), NULL, 0, false);
        char* name = concat((char*)m->type->structure.id->name, "-init");
        LLVMV init = LLVMAddFunction(irs->M, name, T);
        free(name);
        bb = LLVMAppendBasicBlock(init, BB_ENTRY);
        LLVMPositionBuilderAtEnd(irs->B, bb);
        list_append(inits, (ListValue)init);
    }

    { // methods, constructor and VMTs (R, L & P)
        LLVMV R,  L,  P;  // VMTs
        LLVMV VR, VL, VP; // functions

        m->type->structure.gR = R = vmtadd(irs->M, m, "-vmt-R");
        m->type->structure.gL = L = vmtadd(irs->M, m, "-vmt-L");
        m->type->structure.gP = P = vmtadd(irs->M, m, "-vmt-P");

        // constructor
        irs->structure = m;
        backend_definition(irs, m->type->structure.constructor);
        irs->structure = NULL;

        int j, k;
        LLVMT selfT = m->type->T;
        LLVMT proxyT = proxytype(m->type->structure.id->name, all);
        for (int i = j = (k = public, 0); i < all; i++) {
            if (!ISPRIVATE(methods[i])) {
                VR = fnR(irs, methods[i], selfT);
                VL = fnL(irs, methods[i], selfT);
                VP = fnP(irs, methods[i], proxyT);
                LLVMPositionBuilderAtEnd(irs->B, bb);
                vmtset(irs->B, R, VR, j);
                vmtset(irs->B, L, VL, j);
                vmtset(irs->B, P, VP, j);
                methods[i]->function.vmti = j++;
            } else {
                VR = fnR(irs, methods[i], selfT);
                LLVMPositionBuilderAtEnd(irs->B, bb);
                vmtset(irs->B, R, VR, k);
                methods[i]->function.vmti = k++;
            }
        }

        m->type->structure.proxyT = proxyT;
    }

    LLVMPositionBuilderAtEnd(irs->B, bb);
    LLVMBuildRetVoid(irs->B);
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

    irsBB_start(irs, LLVMAppendBasicBlock(fn->V, BB_ENTRY));

    irs->function = fn->V;
}

// auxiliary - adds a global value (to be initialized later)
static void addglobal(IRState* irs, Definition* d) {
    LLVMT T = llvmtype(d->capsa.capsa->type);
    d->capsa.capsa->V = LLVMAddGlobal(irs->M, T, d->capsa.capsa->id->name);
    LLVMSetInitializer(d->capsa.capsa->V, LLVMGetUndef(T));
    list_append(globals, d);
}

// auxiliary - initializes all global values
static void initializeglobals(IRState* irs) {
    FOREACH(ListNode, n, globals->first) {
        Definition* d = (Definition*)n->value;
        backend_expression(irs, d->capsa.expression);
        LLVMBuildStore(irs->B, d->capsa.expression->V, d->capsa.capsa->V);
    }
}

// auxiliary - creates readable names for functions
// kind => {0 : regular, 1 : locking, 2 : proxy}
static const char* fname(Definition* fn, int kind) {
    char* s;
    int len = strlen(fn->function.id->name);
    if (fn->function.qualifiers & FQ_ACQUIRE) {
        len += 8;
        SMALLOC(s, len);
        strcpy(s, "acquire-");
    } else if (fn->function.qualifiers & FQ_RELEASE) {
        len += 8;
        SMALLOC(s, len);
        strcpy(s, "release-");
    } else {
        SMALLOC(s, len);
        s[0] = '\0';
    }

    strcat(s, fn->function.id->name);
    char* name = concat(s,
          kind == 0 ? "-R"
        : kind == 1 ? "-L"
        : kind == 2 ? "-P"
        : (INVALID  , "-error")
    );
    free(s);

    return name;
}

// TODO: doc
static LLVMV fnR(IRState* irs, Definition* fn, LLVMT selfT) {
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

    irsBB_start(irs, LLVMAppendBasicBlock(fn->V, BB_ENTRY));
    irs->function = fn->V;

    // <self> is the first parameter
    selfT = irT_ptr(selfT);
    irs->self = fn->function.parameters->capsa.capsa->V;
    irs->self = LLVMBuildBitCast(irs->B, irs->self, selfT, LLVM_TMP_SELF);

    // native functions
    // TODO: separate function for native functions
    if (fn->function.id->name == scanner_native[SCANNER_NATIVE_UNLOCKED]) {
        // NOTE: the acquire-value statement already calls lock/unlock
        //       this block is left here for future use
        // LLVMV mutex = monitormutex(irs->B, irs->self);
        if (fn->function.qualifiers & FQ_ACQUIRE) {
            // irPT_mutex_lock(irs->B, mutex);
            LLVMBuildRet(irs->B, fn->function.parameters->capsa.capsa->V);
        } else if (fn->function.qualifiers & FQ_RELEASE) {
            // irPT_mutex_unlock(irs->B, mutex);
            LLVMBuildRetVoid(irs->B);
        } else {
            UNREACHABLE;
        }
        irs->block = NULL;
    } else {
        backend_block(irs, fn->function.block);
        // checks if it's necessary to place an implicit return instruction
        if (irs->block) {
            irs_return(irs, llvmzero(fn->function.type));
        }
    }

    irs->self = NULL;
    return fn->V;
}

// TODO: doc
static LLVMV fnL(IRState* irs, Definition* fn, LLVMT selfT) {
    assert(fn->V);

    // unlocked function
    if (fn->function.id->name == scanner_native[SCANNER_NATIVE_UNLOCKED]) {
        return fn->V;;
    }

    // adds the locking-function's prototype to the module
    int argc = LLVMCountParams(fn->V);
    const char* name = fname(fn, 1);
    LLVMV fnL = LLVMAddFunction(irs->M, name, llvm_fn_type(fn, argc));
    free((void*)name);

    irsBB_start(irs, LLVMAppendBasicBlock(fnL, BB_ENTRY));

    // gets the monitor's mutex from <self>
    LLVMV selfV = LLVMGetParam(fnL, 0);
    selfT = irT_ptr(selfT);
    selfV = LLVMBuildBitCast(irs->B, selfV, selfT, LLVM_TMP_SELF);
    LLVMV mutex = monitormutex(irs->B, selfV);

    irPT_mutex_lock(irs->B, mutex);

    // TODO: when calling the R function, should pass the M! object
    //       with the unlocked VMT

    // calls the R function
    LLVMV argv[argc];
    LLVMGetParams(fnL, argv);
    LLVMV ret = (fn->function.type != __void)
        ? LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_RETURN)
        : (LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_NONE), NULL)
        ;

    irPT_mutex_unlock(irs->B, mutex);

    irs_return(irs, ret ? ret : llvmzero(fn->function.type));

    return fnL;
}

// TODO: test generic function proxy with variadic parameters
static LLVMV fnP(IRState* irs, Definition* fn, LLVMT proxyT) {
    // adds the proxy function's prototype to the module
    int argc = LLVMCountParams(fn->V);
    const char* name = fname(fn, 2);
    LLVMV fnP = LLVMAddFunction(irs->M, name, llvm_fn_type(fn, argc));
    free((void*)name);

    LLVMPositionBuilderAtEnd(irs->B, LLVMAppendBasicBlock(fnP, BB_ENTRY));

    LLVMV V;
    LLVMT T;

    // gets the <proxy> from the first argument
    V = LLVMGetParam(fnP, 0);
    T = irT_ptr(proxyT);
    LLVMV proxy = LLVMBuildBitCast(irs->B, V, T, LLVM_TMP_PROXY);

    // checks if <proxy.ok> is true
    LLVMBB bberr = LLVMAppendBasicBlock(fnP, "error");
    LLVMBB bbok = LLVMAppendBasicBlock(fnP, "ok");
    V = LLVMBuildStructGEP(irs->B, proxy, PROXY_IDX_OK, LLVM_TMP_OK);
    V = LLVMBuildLoad(irs->B, V, LLVM_TMP_OK);
    V = LLVMBuildICmp(irs->B, LLVMIntEQ, V, ir_bool(true), LLVM_TMP);
    LLVMBuildCondBr(irs->B, V, bbok, bberr);
    LLVMPositionBuilderAtEnd(irs->B, bberr);
    // TODO: do this right
    const char* msg = "runtime error: proxy function not ok\n";
    LLVMV err = LLVMBuildGlobalString(irs->B, msg, "msg");
    err = LLVMBuildBitCast(irs->B, err, irT_pvoid, "msg");
    LLVMV error[] = {err};
    ir_printf(irs->B, error, 1);
    // end TODO
    ir_exit(irs->B);
    LLVMBuildUnreachable(irs->B);
    LLVMPositionBuilderAtEnd(irs->B, bbok);

    // calls the function from the regular VMT
    LLVMV argv[argc];
    LLVMGetParams(fnP, argv);
    V = LLVMBuildStructGEP(irs->B, proxy, PROXY_IDX_OBJ, LLVM_TMP_SELF);
    V = LLVMBuildLoad(irs->B, V, LLVM_TMP_SELF);
    argv[0] = V;
    if (fn->function.type != __void) { // TODO: direct calls not using the VMT
        V = LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_RETURN);
        LLVMBuildRet(irs->B, V);
    } else {
        LLVMBuildCall(irs->B, fn->V, argv, argc, LLVM_TMP_NONE);
        LLVMBuildRetVoid(irs->B);
    }

    return fnP;
}

// -----------------------------------------------------------------------------

// auxiliary - adds a new global variable for a VMT
static LLVMV vmtadd(LLVMM M, Definition* m, char* name) {
    // FIXME: L and R have the same array size (despite the private functions)
    int n = m->type->structure.methods_size;
    LLVMT T = LLVMArrayType(irT_pvoid, n);
    char* s = concat((char*)m->type->structure.id->name, name);
    LLVMV V = LLVMAddGlobal(M, T, s);
    free(s);
    LLVMSetInitializer(V, LLVMGetUndef(T));
    return V;
}

// auxiliary - stores a function in a given position of a VMT
static void vmtset(LLVMB B, LLVMV vmt, LLVMV fn, int n) {
    assert(vmt && fn);
    LLVMV indices[] = {ir_int(0), ir_int(n)};
    LLVMV P = LLVMBuildGEP(B, vmt, indices, 2, LLVM_TMP);
    LLVMV V = LLVMBuildBitCast(B, fn, irT_pvoid, LLVM_TMP);
    LLVMBuildStore(B, V, P);
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

static void backend_stmt_assignment(IRState*, Statement*);
static void backend_stmt_function_call(IRState*, Statement*);
static void backend_stmt_wait_for_in(IRState*, Statement*);
static void backend_stmt_signal(IRState*, Statement*);
static void backend_stmt_broadcast(IRState*, Statement*);
static void backend_stmt_return(IRState*, Statement*);
static void backend_stmt_if(IRState*, Statement*);
static void backend_stmt_if_else(IRState*, Statement*);
static void backend_stmt_while(IRState*, Statement*);
static void backend_stmt_numeric_for(IRState*, Statement*);
static void backend_stmt_for(IRState*, Statement*);
static void backend_stmt_spawn(IRState*, Statement*);
static void backend_stmt_acquire_value(IRState*, Statement*);
static void backend_stmt_block(IRState*, Statement*);

static void spawnfunction(IRState*, Definition*, LLVMT);
static LLVMBB forin(IRState*, Def*, Exp*, LLVMBB, LLVMBB);

static void backend_statement(IRState* irs, Statement* stmt) {
    switch (stmt->tag) {
    case STATEMENT_ASSIGNMENT:    backend_stmt_assignment(irs, stmt);    break;
    case STATEMENT_FUNCTION_CALL: backend_stmt_function_call(irs, stmt); break;
    case STATEMENT_WAIT_FOR_IN:   backend_stmt_wait_for_in(irs, stmt);   break;
    case STATEMENT_SIGNAL:        backend_stmt_signal(irs, stmt);        break;
    case STATEMENT_BROADCAST:     backend_stmt_broadcast(irs, stmt);     break;
    case STATEMENT_RETURN:        backend_stmt_return(irs, stmt);        break;
    case STATEMENT_IF:            backend_stmt_if(irs, stmt);            break;
    case STATEMENT_IF_ELSE:       backend_stmt_if_else(irs, stmt);       break;
    case STATEMENT_WHILE:         backend_stmt_while(irs, stmt);         break;
    case STATEMENT_NUMERIC_FOR:   backend_stmt_numeric_for(irs, stmt);   break;
    case STATEMENT_FOR:           backend_stmt_for(irs, stmt);           break;
    case STATEMENT_SPAWN:         backend_stmt_spawn(irs, stmt);         break;
    case STATEMENT_ACQUIRE_VALUE: backend_stmt_acquire_value(irs, stmt); break;
    case STATEMENT_BLOCK:         backend_stmt_block(irs, stmt);         break;
    default:
        UNREACHABLE;
    }
}

static void backend_stmt_assignment(IRState* irs, Statement* stmt) {
    Capsa* capsa = stmt->assignment.capsa;
    Expression* exp = stmt->assignment.expression;
    backend_capsa(irs, capsa);
    backend_expression(irs, exp);
    LLVMBuildStore(irs->B, exp->V, capsa->V);
}

static void backend_stmt_function_call(IRState* irs, Statement* stmt) {
    backend_function_call(irs, stmt->function_call);
}

static void backend_stmt_wait_for_in(IRState* irs, Statement* stmt) {
    LLVMBB bbcond = LLVMAppendBasicBlock(irs->function, BB_COND);
    LLVMBB bbloop = LLVMAppendBasicBlock(irs->function, BB_LOOP);
    LLVMBB bbend  = LLVMAppendBasicBlock(irs->function, BB_END);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);
    // cond
    irsBB_start(irs, bbcond);
    backend_condition(irs, stmt->wait_for_in.condition, bbend, bbloop);
    // loop
    irsBB_start(irs, bbloop);
    backend_expression(irs, stmt->wait_for_in.queue);
    LLVMV mutex = monitormutex(irs->B, irs->self);
    irPT_cond_wait(irs->B, stmt->wait_for_in.queue->V, mutex);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);
    // end
    irsBB_start(irs, bbend);
}

static void backend_stmt_signal(IRState* irs, Statement* stmt) {
    backend_expression(irs, stmt->signal);
    irPT_cond_signal(irs->B, stmt->signal->V);
}

static void backend_stmt_broadcast(IRState* irs, Statement* stmt) {
    backend_expression(irs, stmt->broadcast);
    irPT_cond_broadcast(irs->B, stmt->broadcast->V);
}

static void backend_stmt_return(IRState* irs, Statement* stmt) {
    if (stmt->return_) {
        backend_expression(irs, stmt->return_);
        irs_return(irs, stmt->return_->V);
    } else {
        irs_return(irs, NULL);
    }
}

static void backend_stmt_if(IRState* irs, Statement* stmt) {
    LLVMBB bbif  = LLVMAppendBasicBlock(irs->function, BB_IF);
    LLVMBB bbend = LLVMAppendBasicBlock(irs->function, BB_END);
    backend_condition(irs, stmt->if_.expression, bbif, bbend);
    // if
    irsBB_start(irs, bbif);
    backend_block(irs, stmt->if_.block);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbend);
        irsBB_end(irs);
    }
    // end
    irsBB_start(irs, bbend);
}

static void backend_stmt_if_else(IRState* irs, Statement* stmt) {
    LLVMBB bbif   = LLVMAppendBasicBlock(irs->function, BB_IF);
    LLVMBB bbelse = LLVMAppendBasicBlock(irs->function, BB_ELSE);
    LLVMBB bbend  = LLVMAppendBasicBlock(irs->function, BB_END);
    backend_condition(irs, stmt->if_else.expression, bbif, bbelse);
    // if
    irsBB_start(irs, bbif);
    backend_block(irs, stmt->if_else.if_block);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbend);
        irsBB_end(irs);
    }
    // else
    irsBB_start(irs, bbelse);
    backend_block(irs, stmt->if_else.else_block);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbend);
        irsBB_end(irs);
    }
    // end
    irsBB_start(irs, bbend);
}

static void backend_stmt_while(IRState* irs, Statement* stmt) {
    LLVMBB bbcond = LLVMAppendBasicBlock(irs->function, BB_COND);
    LLVMBB bbloop = LLVMAppendBasicBlock(irs->function, BB_LOOP);
    LLVMBB bbend  = LLVMAppendBasicBlock(irs->function, BB_END);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);
    // cond
    irsBB_start(irs, bbcond);
    backend_condition(irs, stmt->while_.expression, bbloop, bbend);
    // loop
    irsBB_start(irs, bbloop);
    backend_block(irs, stmt->while_.block);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbcond);
        irsBB_end(irs);
    }
    // end
    irsBB_start(irs, bbend);
}

static void backend_stmt_numeric_for(IRState* irs, Statement* stmt) {
    LLVMBB bbloop = LLVMAppendBasicBlock(irs->function, BB_LOOP);
    LLVMBB bbend  = LLVMAppendBasicBlock(irs->function, BB_END);

    Def* v = stmt->numeric_for.v;
    Exp* range = stmt->numeric_for.range;

    v->capsa.expression = range->range.first;
    backend_definition(irs, v); // calls <backend_expression> for first
    backend_expression(irs, range->range.last);
    if (range->range.second) {
        backend_expression(irs, range->range.second);
    }
    LLVMBB bbinc = forin(irs, v, range, bbloop, bbend);

    // loop
    irsBB_start(irs, bbloop);
    backend_block(irs, stmt->numeric_for.block);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbinc);
        irsBB_end(irs);
    }

    // end
    irsBB_start(irs, bbend);
}

static void backend_stmt_for(IRState* irs, Statement* stmt) {
    LLVMBB bbcond = LLVMAppendBasicBlock(irs->function, BB_COND);
    LLVMBB bbloop = LLVMAppendBasicBlock(irs->function, BB_LOOP);
    LLVMBB bbend  = LLVMAppendBasicBlock(irs->function, BB_END);
    backend_definition(irs, stmt->for_.initialization);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);
    // cond
    irsBB_start(irs, bbcond);
    backend_condition(irs, stmt->for_.condition, bbloop, bbend);
    // loop
    irsBB_start(irs, bbloop);
    backend_block(irs, stmt->for_.block);
    backend_statement(irs, stmt->for_.increment);
    if (irs->block) {
        LLVMBuildBr(irs->B, bbcond);
        irsBB_end(irs);
    }
    // end
    irsBB_start(irs, bbend);
}

// TODO: test
static void backend_stmt_spawn(IRState* irs, Statement* stmt) {
    // arguments
    LLVMT argsT = irT_pvoid;
    LLVMV argsV = ir_zeroptr;
    if (stmt->spawn->argc > 0) {
        int i = 0;
        LLVMT Ts[stmt->spawn->argc];
        FOREACH(Expression, e, stmt->spawn->arguments) {
            backend_expression(irs, e);
            Ts[i++] = LLVMTypeOf(e->V);
        }
        argsT = LLVMStructType(Ts, stmt->spawn->argc, false);
        argsV = LLVMGetUndef(argsT);
        FOREACH(Expression, e, (i = 0, stmt->spawn->arguments)) {
            argsV = LLVMBuildInsertValue(irs->B, argsV, e->V, i++, LLVM_TMP);
        }
        LLVMV malloc = LLVMBuildMalloc(irs->B, argsT, LLVM_TMP);
        LLVMBuildStore(irs->B, argsV, malloc);
        argsT = irT_ptr(argsT);
        argsV = malloc;
    }

    // pthread_create(...)
    LLVMV spawn = LLVMAddFunction(irs->M, "spawn", irPTT_spawn);
    LLVMV args = LLVMBuildBitCast(irs->B, argsV, irT_pvoid, LLVM_TMP);
    irPT_create(irs->B, spawn, args);

    // spawn function
    IRState* irss = irs_new(irs->M, irs->B);
    irss->function = spawn;
    spawnfunction(irss, stmt->spawn->fn, argsT);
    irs_destroy(irss);

    LLVMPositionBuilderAtEnd(irs->B, irs->block);
}

static void backend_stmt_acquire_value(IRState* irs, Statement* stmt) {
    // calls the acquire function (m.acquire-f)
    Definition* d = stmt->acquire_value.value;
    backend_definition(irs, stmt->acquire_value.value);

    // acquire value sv = m.f() { ... }
    FunctionCall* f   = d->capsa.expression->function_call; // method
    Expression*   m   = f->obj;                             // monitor object
    Capsa*        sv  = d->capsa.capsa;                     // scoped value

    LLVMV V;

    // creates the proxy object for <sv>
    Type* t = sv->type->unlocked;
    LLVMV proxy = LLVMBuildMalloc(irs->B, t->structure.proxyT, LLVM_TMP_PROXY);
    // proxy.m = m
    V = LLVMBuildStructGEP(irs->B, proxy, PROXY_IDX_OBJ, LLVM_TMP_OBJ);
    LLVMBuildStore(irs->B, sv->V, V);
    // proxy.vmt = vmtP
    V = LLVMBuildStructGEP(irs->B, proxy, PROXY_IDX_VMTP, LLVM_TMP_VMT);
    LLVMBuildStore(irs->B, t->structure.gP, V);
    // proxy.ok = true
    LLVMV ok = LLVMBuildStructGEP(irs->B, proxy, PROXY_IDX_OK, LLVM_TMP_OK);
    LLVMBuildStore(irs->B, ir_bool(true), ok);

    proxy = LLVMBuildBitCast(irs->B, proxy, irT_pvoid, LLVM_TMP_PROXY);

    // locks <sv>
    V = LLVMBuildBitCast(irs->B, sv->V, irT_ptr(t->T), LLVM_TMP_OBJ);
    LLVMV mutex = monitormutex(irs->B, V);
    irPT_mutex_lock(irs->B, mutex);

    sv->V = proxy;
    backend_block(irs, stmt->acquire_value.block);

    // proxy.ok = false
    LLVMBuildStore(irs->B, ir_bool(false), ok);
    // unlocks <sv>
    irPT_mutex_unlock(irs->B, mutex);

    // calls the release function (m.release-f)
    Definition* release = f->fn->function.pair;
    V = LLVMBuildBitCast(irs->B, m->V, irT_ptr(m->type->T), LLVM_TMP_PROXY);
    LLVMV vmt = getvmt(irs->B, V);
    LLVMV fn = ir_array_get(irs->B, vmt, release->function.vmti);
    LLVMT T = llvm_fn_type(release, 1);
    fn = LLVMBuildBitCast(irs->B, fn, irT_ptr(T), LLVM_TMP);
    LLVMV args[] = {m->V};
    LLVMBuildCall(irs->B, fn, args, 1, LLVM_TMP_NONE);
}

static void backend_stmt_block(IRState* irs, Statement* stmt) {
    backend_block(irs, stmt->block);
}

// -----------------------------------------------------------------------------

// auxiliary - defines the LLVM function for a <spawn>
static void spawnfunction(IRState* irs, Definition* spawn, LLVMT psT) {
    int i = 0;
    irsBB_start(irs, LLVMAppendBasicBlock(irs->function, BB_ENTRY));
    LLVMV parameter = LLVMGetParam(irs->function, 0);
    LLVMV psV = parameter;
    psV = LLVMBuildBitCast(irs->B, psV, psT, LLVM_TMP);
    psV = LLVMBuildLoad(irs->B, psV, LLVM_TMP);
    FOREACH(Definition, p, spawn->function.parameters) {
        p->capsa.capsa->V = LLVMBuildExtractValue(irs->B, psV, i++, LLVM_TMP);
    }
    LLVMBuildFree(irs->B, parameter);
    backend_block(irs, spawn->function.block);
    LLVMBuildRet(irs->B, ir_zeroptr);
    irsBB_end(irs);
}

// auxiliary - creates the loop for range expressions
// (used by the "for" statement and list comprehension expressions)
static LLVMBB forin(IRState* irs, Def* v, Exp* range, LLVMBB loop, LLVMBB end) {
    LLVMBB bbcond = LLVMAppendBasicBlock(irs->function, BB_COND);
    LLVMBB bbinc  = LLVMAppendBasicBlock(irs->function, BB_INC);

    int predicates[2][4] = {
        {LLVMIntSLT, LLVMIntSGT, LLVMIntSLE, LLVMIntSGE},
        {LLVMRealOLT, LLVMRealOGT, LLVMRealOLE, LLVMRealOGE}
    };
    LLVMV ratios[2][4] = {
        {ir_int(1), ir_int(-1), ir_int(1), ir_int(-1)},
        {ir_float(1), ir_float(-1), ir_float(1), ir_float(-1)}
    };
    bool isfloat = range->range.first->type == __float; // i
    int j = range->range.op == TK_RARROW  ? 0 :
            range->range.op == TK_LARROW  ? 1 :
            range->range.op == TK_REARROW ? 2 :
            range->range.op == TK_LEQUAL  ? 3 :
            (INVALID, -1);
    int predicate = predicates[isfloat][j]; // LLVM(Int/Real)Predicate
    LLVMV ratio = ratios[isfloat][j];
    if (range->range.second) {
        Exp* first = range->range.first;
        Exp* second = range->range.second;
        ratio = (isfloat)
            ? LLVMBuildFSub(irs->B, second->V, first->V, LLVM_TMP)
            : LLVMBuildSub(irs->B, second->V, first->V, LLVM_TMP);
    }

    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);

    // cond
    irsBB_start(irs, bbcond);
    LLVMV V;
    V = LLVMBuildLoad(irs->B, v->capsa.capsa->V, LLVM_TMP);
    V = (isfloat)
        ? LLVMBuildFCmp(irs->B, predicate, V, range->range.last->V, LLVM_TMP)
        : LLVMBuildICmp(irs->B, predicate, V, range->range.last->V, LLVM_TMP);
    LLVMBuildCondBr(irs->B, V, loop, end);
    irsBB_end(irs);

    // inc
    irsBB_start(irs, bbinc);
    V = LLVMBuildLoad(irs->B, v->capsa.capsa->V, LLVM_TMP);
    V = (isfloat)
        ? LLVMBuildFAdd(irs->B, V, ratio, LLVM_TMP)
        : LLVMBuildAdd(irs->B, V, ratio, LLVM_TMP);
    LLVMBuildStore(irs->B, V, v->capsa.capsa->V);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);

    return bbinc;
}

// ==================================================
//
//  Capsa
//
// ==================================================

static void backend_capsa_id(IRState*, Capsa*);
static void backend_capsa_indexed(IRState*, Capsa*);

static void backend_capsa(IRState* irs, Capsa* capsa) {
    switch (capsa->tag) {
    case CAPSA_ID:        backend_capsa_id(irs, capsa);      break;
    case CAPSA_INDEXED:   backend_capsa_indexed(irs, capsa); break;
    default:
        UNREACHABLE;
    }
}

static void backend_capsa_id(IRState* irs, Capsa* capsa) {
    // V dealt with already, unless...
    if (capsa->llvm_structure_index > -1) { // attributes
        assert(irs->self);
        capsa->V = LLVMBuildStructGEP(
            irs->B, irs->self, capsa->llvm_structure_index, LLVM_TMP
        );
    }
}

static void backend_capsa_indexed(IRState* irs, Capsa* capsa) {
    backend_expression(irs, capsa->indexed.array);
    backend_expression(irs, capsa->indexed.index);
    LLVMV indices[1] = {capsa->indexed.index->V};
    capsa->V = LLVMBuildGEP(irs->B,
        capsa->indexed.array->V, indices, 1, LLVM_TMP
    );
}

// ==================================================
//
//  Expression
//
// ==================================================

static void backend_exp_literal_boolean(IRState*, Exp*);
static void backend_exp_literal_integer(IRState*, Exp*);
static void backend_exp_literal_float(IRState*, Exp*);
static void backend_exp_literal_string(IRState*, Exp*);
static void backend_exp_literal_array(IRState*, Exp*);
static void backend_exp_capsa(IRState*, Exp*);
static void backend_exp_fc(IRState*, Exp*);
static void backend_exp_comprehension(IRState*, Exp*);
static void backend_exp_range(IRState*, Exp*);
static void backend_exp_unary(IRState*, Exp*);
static void backend_exp_unary_minus(IRState*, Exp*);
static void backend_exp_binary(IRState*, Exp*);
static void backend_exp_cast(IRState*, Exp*);
static void backend_exp_condition(IRState*, Exp*);

static LLVMV range_r(IRState*, Token, Exp*, Exp*);
static LLVMV range_n(IRState*, Token, Exp*, Exp*, LLVMV);
static LLVMV range_v(LLVMB, LLVMV, LLVMV);

// macro to be used by the [+, -, *, /] operations
#define BINARY_ARITHMETICS(e, irs, ifunc, ffunc) STMT(\
    if (e->type == __integer) { \
        e->V = ifunc(irs->B, \
            e->binary.left_expression->V, \
            e->binary.right_expression->V, LLVM_TMP); \
    } else if (e->type == __float) { \
        e->V = ffunc(irs->B, \
            e->binary.left_expression->V, \
            e->binary.right_expression->V, LLVM_TMP); \
    } else { \
        UNREACHABLE; \
    } \
)
// end macro

static void backend_expression(IRState* irs, Exp* exp) {
    switch (exp->tag) {
    case EXP_LITERAL_BOOLEAN:    backend_exp_literal_boolean(irs, exp); break;
    case EXP_LITERAL_INTEGER:    backend_exp_literal_integer(irs, exp); break;
    case EXP_LITERAL_FLOAT:      backend_exp_literal_float(irs, exp);   break;
    case EXP_LITERAL_STRING:     backend_exp_literal_string(irs, exp);  break;
    case EXP_LITERAL_ARRAY:      backend_exp_literal_array(irs, exp);   break;
    case EXP_CAPSA:              backend_exp_capsa(irs, exp);           break;
    case EXP_FUNCTION_CALL:      backend_exp_fc(irs, exp);              break;
    case EXP_COMPREHENSION:      backend_exp_comprehension(irs, exp);   break;
    case EXP_RANGE:              backend_exp_range(irs, exp);           break;
    case EXP_UNARY:              backend_exp_unary(irs, exp);           break;
    case EXP_BINARY:             backend_exp_binary(irs, exp);          break;
    case EXP_CAST:               backend_exp_cast(irs, exp);            break;
    default:
        UNREACHABLE;
    }
}

static void backend_exp_literal_boolean(IRState* irs, Exp* exp) {
    exp->V = ir_bool(exp->literal.boolean);
}

static void backend_exp_literal_integer(IRState* irs, Exp* exp) {
    exp->V = ir_int(exp->literal.integer);
}

static void backend_exp_literal_float(IRState* irs, Exp* exp) {
    exp->V = ir_float(exp->literal.float_);
}

static void backend_exp_literal_string(IRState* irs, Exp* exp) {
    exp->V = LLVMBuildGlobalStringPtr(
        irs->B, exp->literal.string, LLVM_GLOBAL_STRING
    );
}

static void backend_exp_literal_array(IRState* irs, Exp* exp) {
    // evaluating the expressions inside the array
    int n = 0;
    for (Exp* e = exp->literal.array; e; e = e->next, n++) {
        backend_expression(irs, e);
    }
    // allocating memory for the array
    exp->V = LLVMBuildArrayMalloc(irs->B,
        llvmtype(exp->type->array), ir_int(n), LLVM_TMP
    );
    // setting the values inside the array
    n = 0;
    LLVMV ptr, indices[1];
    FOREACH(Exp, e, exp->literal.array) {
        indices[0] = ir_int(n++);
        ptr = LLVMBuildGEP(irs->B, exp->V, indices, 1, LLVM_TMP);
        LLVMBuildStore(irs->B, e->V, ptr);
    }
}

static void backend_exp_capsa(IRState* irs, Exp* exp) {
    backend_capsa(irs, exp->capsa);
    // TODO: find better way to write this
    if (exp->capsa->llvm_structure_index > -1) { // attributes
        exp->V = LLVMBuildLoad(irs->B, exp->capsa->V, LLVM_TMP);
    } else if (exp->capsa->value && !exp->capsa->global) { // local values
        exp->V = exp->capsa->V;
    } else { // variables
        exp->V = LLVMBuildLoad(irs->B, exp->capsa->V, LLVM_TMP);
    }
}

static void backend_exp_fc(IRState* irs, Exp* exp) {
    exp->V = backend_function_call(irs, exp->function_call);
}

// TODO
static LLVMV ir_num_range1(IRState* irs, Token op, Exp* first, Exp* last) {
    LLVMV r, n;

    switch (op) {
    case TK_RARROW:
        n = LLVMBuildSub(irs->B, last->V, first->V, LLVM_TMP);
        r = ir_int(1);
        break;
    case TK_REARROW:
        n = LLVMBuildSub(irs->B, last->V, first->V, LLVM_TMP);
        n = LLVMBuildAdd(irs->B, n, ir_int(1), LLVM_TMP);
        r = ir_int(1);
        break;
    case TK_LARROW:
        n = LLVMBuildSub(irs->B, first->V, last->V, LLVM_TMP);
        r = ir_int(-1);
        break;
    case TK_LEARROW:
        n = LLVMBuildSub(irs->B, first->V, last->V, LLVM_TMP);
        n = LLVMBuildAdd(irs->B, n, ir_int(1), LLVM_TMP);
        r = ir_int(-1);
        break;
    default:
        UNREACHABLE;
    }

    LLVMBB bberr = LLVMAppendBasicBlock(irs->function, "size-err");
    LLVMBB bbok = LLVMAppendBasicBlock(irs->function, "size-ok");

    LLVMV sizeok = LLVMBuildICmp(irs->B, LLVMIntSGT, n, ir_int(0), "sizeok");
    LLVMBuildCondBr(irs->B, sizeok, bbok, bberr);
    irsBB_end(irs);
    // size-err
    irsBB_start(irs, bberr);
    ir_runtime_err(irs->B, RERR_COMP_SIZE);
    irsBB_end(irs);
    // size-ok
    irsBB_start(irs, bbok);
    return range_v(irs->B, r, n);
}

static void backend_exp_comprehension(IRState* irs, Exp* exp) {
    // TODO
    // evaluates the range expression
    Exp* range = exp->comprehension.iterable;
    backend_exp_range(irs, range);
    LLVMV r = LLVMBuildExtractValue(irs->B, range->V, 0, "r");
    LLVMV n = LLVMBuildExtractValue(irs->B, range->V, 1, "n");

    // TODO
    // evaluates the definition
    Def* def = exp->comprehension.v;
    Capsa* capsa = def->capsa.capsa;
    Exp* first = range->range.first;
    capsa->value = false;
    backend_definition(irs, def);
    def->capsa.expression = first;
    LLVMBuildStore(irs->B, first->V, capsa->V);

    // allocates the array
    LLVMT T = llvmtype(exp->type->array);
    exp->V = LLVMBuildArrayMalloc(irs->B, T, n, LLVM_TMP);

    LLVMBB bbcond = LLVMAppendBasicBlock(irs->function, BB_COND);
    LLVMBB bbloop = LLVMAppendBasicBlock(irs->function, BB_LOOP);
    LLVMBB bbinc = LLVMAppendBasicBlock(irs->function, BB_INC);
    LLVMBB bbend = LLVMAppendBasicBlock(irs->function, BB_END);

    // ends the previous block
    LLVMBB bbprev = irs->block;
    irsBB_end(irs);
    LLVMBuildBr(irs->B, bbcond);

    // cond (1)
    irsBB_start(irs, bbcond);
    LLVMV iphi = LLVMBuildPhi(irs->B, irT_int, LLVM_TMP);
    irsBB_end(irs);
    // inc
    irsBB_start(irs, bbinc);
    LLVMV i2 = LLVMBuildAdd(irs->B, iphi, ir_int(1), LLVM_TMP);
    LLVMV V = LLVMBuildLoad(irs->B, capsa->V, LLVM_TMP);
    V = LLVMBuildAdd(irs->B, V, r, LLVM_TMP);
    LLVMBuildStore(irs->B, V, capsa->V);
    LLVMBuildBr(irs->B, bbcond);
    irsBB_end(irs);
    // cond (2)
    irsBB_start(irs, bbcond);
    LLVMV incomingV[2] = {ir_int(0), i2};
    LLVMBB incomingBB[2] = {bbprev, bbinc};
    LLVMAddIncoming(iphi, incomingV, incomingBB, 2);
    LLVMV cond = LLVMBuildICmp(irs->B, LLVMIntSLT, iphi, n, LLVM_TMP);
    LLVMBuildCondBr(irs->B, cond, bbloop, bbend);
    irsBB_end(irs);
    // loop
    irsBB_start(irs, bbloop);
    backend_expression(irs, exp->comprehension.e);
    LLVMV indices[] = {iphi};
    LLVMV ptr = LLVMBuildGEP(irs->B, exp->V, indices, 1, LLVM_TMP);
    LLVMBuildStore(irs->B, exp->comprehension.e->V, ptr);
    LLVMBuildBr(irs->B, bbinc);
    irsBB_end(irs);
    // end
    irsBB_start(irs, bbend);
}

static void backend_exp_range(IRState* irs, Exp* exp) {
    Token op = exp->range.op;
    Exp* first = exp->range.first;
    Exp* second = exp->range.second;
    Exp* last = exp->range.last;

    backend_expression(irs, first);
    backend_expression(irs, last);
    if (!second) {
        exp->V = ir_num_range1(irs, op, first, last);
        return;
    }
    backend_expression(irs, second);

    LLVMV r = range_r(irs, op, first, second);
    LLVMV n = range_n(irs, op, first, last, r);
    exp->V = range_v(irs->B, r, n);
}

static void backend_exp_unary(IRState* irs, Exp* exp) {
    switch (exp->unary.token) {
    case '-':    backend_exp_unary_minus(irs, exp); break;
    case TK_NOT: backend_exp_condition(irs, exp);   break;
    default:
        UNREACHABLE;
    }
}

static void backend_exp_unary_minus(IRState* irs, Exp* exp) {
    backend_expression(irs, exp->unary.expression);
    LLVMV V = exp->unary.expression->V;
    exp->V = (exp->type == __integer) ? LLVMBuildNeg(irs->B, V, LLVM_TMP)  :
             (exp->type == __float)   ? LLVMBuildFNeg(irs->B, V, LLVM_TMP) :
             (INVALID, NULL);
}

static void backend_exp_binary(IRState* irs, Exp* exp) {
    switch (exp->binary.token) {
    case TK_OR:
    case TK_AND:
    case TK_EQUAL:
    case TK_NEQUAL:
    case TK_LEQUAL:
    case TK_GEQUAL:
    case '<':
    case '>':
        backend_exp_condition(irs, exp);
        break;
    case '+':
        backend_expression(irs, exp->binary.left_expression);
        backend_expression(irs, exp->binary.right_expression);
        BINARY_ARITHMETICS(exp, irs, LLVMBuildAdd, LLVMBuildFAdd);
        break;
    case '-':
        backend_expression(irs, exp->binary.left_expression);
        backend_expression(irs, exp->binary.right_expression);
        BINARY_ARITHMETICS(exp, irs, LLVMBuildSub, LLVMBuildFSub);
        break;
    case '*':
        backend_expression(irs, exp->binary.left_expression);
        backend_expression(irs, exp->binary.right_expression);
        BINARY_ARITHMETICS(exp, irs, LLVMBuildMul, LLVMBuildFMul);
        break;
    case '/':
        backend_expression(irs, exp->binary.left_expression);
        backend_expression(irs, exp->binary.right_expression);
        BINARY_ARITHMETICS(exp, irs, LLVMBuildSDiv, LLVMBuildFDiv);
        break;
    default:
        UNREACHABLE;
    }
}

static void backend_exp_cast(IRState* irs, Exp* exp) {
    backend_expression(irs, exp->cast);
    Type* from = exp->cast->type;
    Type* to = exp->type;
    LLVMT T;

    if (from == __integer && to == __float) {
        // integer to float
        T = llvmtype(exp->type);
        exp->V = LLVMBuildSIToFP(irs->B, exp->cast->V, T, LLVM_TMP);
    } else if (from == __float && to == __integer) {
        // float to integer
        T = llvmtype(exp->type);
        exp->V = LLVMBuildFPToSI(irs->B, exp->cast->V, T, LLVM_TMP);
    } else if (from->tag == TYPE_MONITOR && to->tag == TYPE_INTERFACE) {
        // monitor to interface
        exp->V = exp->cast->V;
    } else {
        UNREACHABLE;
    }
}

static void backend_exp_condition(IRState* irs, Exp* exp) {
    // expression ? true : false
    LLVMBB bbtrue  = LLVMAppendBasicBlock(irs->function, "a");
    LLVMBB bbfalse = LLVMAppendBasicBlock(irs->function, "b");
    LLVMBB bbphi   = LLVMAppendBasicBlock(irs->function, "phi");

    backend_condition(irs, exp, bbtrue, bbfalse);
    irsBB_start(irs, bbtrue);
    LLVMBuildBr(irs->B, bbphi);
    irsBB_end(irs);
    irsBB_start(irs, bbfalse);
    LLVMBuildBr(irs->B, bbphi);
    irsBB_end(irs);
    irsBB_start(irs, bbphi);

    LLVMV phi = LLVMBuildPhi(irs->B, irT_bool, LLVM_TMP_PHI);
    LLVMV incomingV[2] = {ir_bool(true), ir_bool(false)};
    LLVMBB incomingBB[2] = {bbtrue, bbfalse};
    LLVMAddIncoming(phi, incomingV, incomingBB, 2);

    exp->V = phi;
}

#undef BINARY_ARITHMETICS

// -----------------------------------------------------------------------------

static LLVMV range_r(IRState* irs, Token op, Exp* first, Exp* second) {
    LLVMBB bbrerr = LLVMAppendBasicBlock(irs->function, "r-err");
    LLVMBB bbrok = LLVMAppendBasicBlock(irs->function, "r-ok");
    LLVMV r = LLVMBuildSub(irs->B, second->V, first->V, LLVM_TMP);
    LLVMIntPredicate predicate = (op == TK_RARROW || op == TK_REARROW)
        ? LLVMIntSGT
        : LLVMIntSLT
        ;
    LLVMV rok = LLVMBuildICmp(irs->B, predicate, r, ir_int(0), "rok");
    LLVMBuildCondBr(irs->B, rok, bbrok, bbrerr);
    irsBB_end(irs);
    // r-err
    irsBB_start(irs, bbrerr);
    ir_runtime_err(irs->B, RERR_RANGE_R);
    irsBB_end(irs);
    // r-ok
    irsBB_start(irs, bbrok);
    return r;
}

static LLVMV range_n(IRState* irs, Token op, Exp* first, Exp* last, LLVMV r) {
    LLVMV n, n1;
    n1 = LLVMBuildSub(irs->B, last->V, first->V, LLVM_TMP);
    n1 = LLVMBuildSDiv(irs->B, n1, r, "nA");

    if (op == TK_RARROW || op == TK_REARROW) {
        LLVMBB bbprev = irs->block;
        LLVMBB bbhasrem = LLVMAppendBasicBlock(irs->function, "has-rem");
        LLVMBB bbnorem = LLVMAppendBasicBlock(irs->function, "no-rem");

        LLVMV rem = LLVMBuildSRem(irs->B, n1, r, "rem");
        LLVMV has = LLVMBuildICmp(irs->B, LLVMIntNE, rem, ir_int(0), "has");
        LLVMBuildCondBr(irs->B, has, bbhasrem, bbnorem);
        irsBB_end(irs);
        // has-rem
        irsBB_start(irs, bbhasrem);
        LLVMV n2 = LLVMBuildAdd(irs->B, n1, ir_int(1), "nB");
        LLVMBuildBr(irs->B, bbnorem);
        irsBB_end(irs);
        // no-rem
        irsBB_start(irs, bbnorem);
        n = LLVMBuildPhi(irs->B, irT_int, "n");
        LLVMV incomingV[] = {n1, n2};
        LLVMBB incomingBB[] = {bbprev, bbhasrem};
        LLVMAddIncoming(n, incomingV, incomingBB, 2);
    } else {
        n = LLVMBuildAdd(irs->B, n1, ir_int(1), "n");
    }

    return n;
}

static LLVMV range_v(LLVMB B, LLVMV r, LLVMV n) {
    LLVMT Ts[] = {irT_int, irT_int};
    LLVMV V = LLVMGetUndef(LLVMStructType(Ts, 2, false));
    V = LLVMBuildInsertValue(B, V, r, 0, LLVM_TMP);
    V = LLVMBuildInsertValue(B, V, n, 1, LLVM_TMP);
    return V;
}

// ==================================================
//
//  Condition
//
// ==================================================

static void backend_condition(IRState* irs, Exp* exp, LLVMBB lt, LLVMBB lf) {
    switch (exp->tag) {
    case EXP_LITERAL_BOOLEAN:
    case EXP_LITERAL_INTEGER:
    case EXP_LITERAL_FLOAT:
    case EXP_LITERAL_STRING:
    case EXP_CAPSA:
    case EXP_FUNCTION_CALL:
        goto EXP_CONDITION;
    case EXP_UNARY:
        switch (exp->unary.token) {
        case TK_NOT:
            backend_condition(irs, exp->unary.expression, lf, lt);
            break;
        case '-':
            goto EXP_CONDITION;
        default:
            UNREACHABLE;
        }
        break;
    case EXP_BINARY: {
        Exp* l = exp->binary.left_expression;
        Exp* r = exp->binary.right_expression;
        LLVMBB label; // used by "or" and "and"

        switch (exp->binary.token) {
        case TK_OR:
            label = LLVMAppendBasicBlock(irs->function, "or");
            backend_condition(irs, l, lt, label);
            irsBB_start(irs, label);
            backend_condition(irs, r, lt, lf);
            break;
        case TK_AND:
            label = LLVMAppendBasicBlock(irs->function, "and");
            backend_condition(irs, l, label, lf);
            irsBB_start(irs, label);
            backend_condition(irs, r, lt, lf);
            break;
        case TK_EQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntEQ, LLVMRealOEQ, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case TK_NEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntNE, LLVMRealONE, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case TK_LEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntSLE, LLVMRealOLE, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case TK_GEQUAL:
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntSGE, LLVMRealOGE, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case '<':
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntSLT, LLVMRealOLT, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case '>':
            backend_expression(irs, l);
            backend_expression(irs, r);
            exp->V = ir_cmp(irs->B, LLVMIntSGT, LLVMRealOGT, l, r);
            LLVMBuildCondBr(irs->B, exp->V, lt, lf);
            irsBB_end(irs);
            break;
        case '+':
        case '-':
        case '*':
        case '/':
            goto EXP_CONDITION;
        default:
            UNREACHABLE;
        }
        break;
    }
    case EXP_CAST:
        goto EXP_CONDITION;
    default:
        UNREACHABLE;
    }

    return;

    EXP_CONDITION: {
        backend_expression(irs, exp);
        LLVMBuildCondBr(irs->B, exp->V, lt, lf);
        irsBB_end(irs);
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
static LLVMV backend_fc_integer_conversion(IRState*, FunctionCall*);
static LLVMV backend_fc_array(IRState*, FunctionCall*);

static LLVMV* fcargs(IRState*, FunctionCall*);
static LLVMV nativefc(IRState*, FunctionCall*);

static LLVMV backend_function_call(IRState* irs, FunctionCall* fc) {
    switch (fc->tag) {
    case FUNCTION_CALL_BASIC:
        return backend_fc_basic(irs, fc);
    case FUNCTION_CALL_METHOD:
        return backend_fc_method(irs, fc);
    case FUNCTION_CALL_CONSTRUCTOR:
        switch (fc->type->tag) {
        case TYPE_ID: // ConditionQueue initializer
            if (fc->type == __condition_queue) {
                return backend_fc_condition_queue(irs, fc);
            } else if (fc->type == __integer) {
                return backend_fc_integer_conversion(irs, fc);
            } else {
                UNREACHABLE;
            }
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
    LLVMT oT = fc->obj->type->T ? fc->obj->type->T : fc->obj->type->unlocked->T;
    assert(oT); // TODO: oT (gambiarra)
    LLVMV obj = LLVMBuildBitCast(irs->B, args[0], irT_ptr(oT), LLVM_TMP);

    // bitcast from i8* to (T)*
    // FIXME: LLVMT T = llvm_fn_type(fc->fn, fc->argc);
    LLVMT T; // function type
    {
        int i = 1;
        LLVMT params[fc->argc]; // parameter types
        params[0] = irT_pvoid;
        FOREACH(Definition, p, fc->fn->function.parameters->next) {
            params[i++] = llvmtype(p->capsa.capsa->type);
        }
        T = llvmtype(fc->fn->function.type); // return type
        T = LLVMFunctionType(T, params, fc->argc, false);
    }

    int vmti = fc->fn->function.vmti;
    if (fc->obj->type->tag == TYPE_INTERFACE) { // FIXME: gambiarra
        vmti += 2; // accounting for the <unlocked> pair
    }

    // VMT function call
    LLVMV vmt = getvmt(irs->B, obj);
    LLVMV fn = ir_array_get(irs->B, vmt, vmti);
    fn = LLVMBuildBitCast(irs->B, fn, irT_ptr(T), LLVM_TMP);
    LLVMV V = LLVMBuildCall(irs->B, fn, args, fc->argc, LLVM_TMP_NONE);

    free(args);
    return V;
}

static LLVMV backend_fc_condition_queue(IRState* irs, FunctionCall* fc) {
    LLVMV V = ir_malloc(irs->B, sizeof(pthread_cond_t));
    irPT_cond_init(irs->B, V);
    return V;
}

static LLVMV backend_fc_integer_conversion(IRState* irs, FunctionCall* fc) {
    backend_expression(irs, fc->arguments);
    LLVMV V = fc->arguments->V;
    if (fc->arguments->type == __integer) {
        // empty
    } else if (fc->arguments->type == __float) {
        V = LLVMBuildFPToSI(irs->B, V, irT_int, LLVM_TMP);
    } else {
        UNREACHABLE;
    }
    return V;
}

static LLVMV backend_fc_array(IRState* irs, FunctionCall* fc) {
    assert(fc->argc == 1);
    Expression* size = fc->arguments;
    backend_expression(irs, size);
    return LLVMBuildArrayMalloc(
        irs->B,
        llvmtype(fc->type->array),
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
    } else if (!strcmp(fc->id->name, "rand")) {
        V = ir_rand(irs->B);
    } else if (!strcmp(fc->id->name, "srand")) {
        V = ir_srand(irs->B, args[0]);
    } else if (!strcmp(fc->id->name, "getTime")) {
        V = ir_getTime(irs->B);
    } else if (!strcmp(fc->id->name, "assert")) {
        V = ir_assert(irs->B, args);
    } else {
        UNREACHABLE;
    }
    free(args);
    return V;
}

// TODO: doc
static LLVMV getvmt(LLVMB B, LLVMV obj) {
    obj = LLVMBuildStructGEP(B, obj, STRUCT_IDX_VMT, LLVM_TMP);
    obj = LLVMBuildLoad(B, obj, LLVM_TMP_VMT);
    return obj;
}

// returns (*ptr)[i], given <ptr> is a pointer to an array
static LLVMV ir_array_get(LLVMB B, LLVMV ptr, int i) {
    LLVMV indices[] = {ir_int(0), ir_int(i)};
    ptr = LLVMBuildGEP(B, ptr, indices, 2, LLVM_TMP);
    return LLVMBuildLoad(B, ptr, LLVM_TMP);
}

// ==================================================
//
//  Auxiliary
//
// ==================================================

// <aria type> to <llvm type>
static LLVMT llvmtype(Type* t) {
    assert(t);
    switch (t->tag) {
    case TYPE_VOID:                  return irT_void;
    case TYPE_ID:
        if (t == __boolean)          return irT_bool;
        if (t == __integer)          return irT_int;
        if (t == __float)            return irT_float;
        if (t == __string)           return irT_string;
        if (t == __condition_queue)  return irPTT_cond;
        UNREACHABLE;
    case TYPE_ARRAY:                 return irT_array(llvmtype(t->array));
    case TYPE_UNLOCKED:              return llvmtype(t->unlocked);
    case TYPE_INTERFACE:             return irT_interface;
    case TYPE_MONITOR:               return irT_monitor;
    default:
        UNREACHABLE;
    }
}

// TODO: ConstantAggregateZero ?
static LLVMV llvmzero(Type* t) {
    switch (t->tag) {
    case TYPE_VOID:
        return NULL; // not unreachable
    case TYPE_ID:
        if (t == __boolean)         return ir_zerobool;
        if (t == __integer)         return ir_zeroint;
        if (t == __float)           return ir_zerofloat;
        if (t == __string)          return ir_zerostring;
        if (t == __condition_queue) return LLVMConstNull(irPTT_cond);
        UNREACHABLE;
    case TYPE_ARRAY:                return LLVMConstNull(llvmtype(t));
    case TYPE_MONITOR:              return LLVMConstNull(llvmtype(t));
    default:
        UNREACHABLE;
    }
}

static LLVMT llvmstruct(LLVMT Ts[], size_t size, const char* name) {
    LLVMT T = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
    LLVMStructSetBody(T, Ts, size, false);
    return T;
}

// given a function definition, returns its LLVM type
static LLVMT llvm_fn_type(Definition* fn, size_t psize) {
    int i = 0;
    LLVMT params[psize];
    FOREACH(Definition, p, fn->function.parameters) {
        params[i++] = llvmtype(p->capsa.capsa->type);
    }
    LLVMT T = llvmtype(fn->function.type);
    return LLVMFunctionType(T, params, psize, false);
}
