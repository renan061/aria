#include <assert.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "alloc.h"
#include "ir.h"
#include "macros.h"

#define NAME_PRINTF "printf"
#define NAME_MALLOC "malloc"
#define NAME_EXIT   "exit"

// module variables
LLVMT
    irT_pvoid,
    irT_void,
    irT_bool,
    irT_int,
    irT_float;
LLVMV
    ir_zerobool,
    ir_zeroint,
    ir_zerofloat;

// internal
static LLVMV
    ir_malloc_t = NULL,
    ir_printf_t = NULL,
    ir_exit_t   = NULL
;

// ==================================================
//
//  IRState
//
// ==================================================

IRState* ir_state_new(LLVMM M, LLVMB B) {
    IRState* irs;
    MALLOC(irs, IRState);
    irs->M = M;
    irs->B = B;
    irs->function = NULL;
    irs->block = NULL;
    irs->structure = NULL;
    irs->self = NULL;
    irs->main = false;
    irs->initializer = false;
    return irs;
}

void ir_state_done(IRState* irs) {
    char* error = NULL;
    LLVMVerifyModule(irs->M, LLVMAbortProcessAction, &error);
    if (error) {
        LLVMDisposeMessage(error);
    }
    LLVMDisposeBuilder(irs->B);
}

void ir_state_free(IRState* irs) {
    free(irs);
}

// ==================================================
//
//  Declares
//
// ==================================================

// TODO: duplicated => also in backend.c
static Type* __boolean;
static Type* __integer;
static Type* __float;

void ir_setup(LLVMM M) {
    { // primitive types
        __boolean = ast_type_boolean();
        __integer = ast_type_integer();
        __float = ast_type_float();
    }

    { // types
        irT_pvoid = irT_ptr(LLVMInt8Type());
        irT_void  = LLVMVoidType();
        irT_bool  = LLVMIntType(1);
        irT_int   = LLVMInt32Type();
        irT_float = LLVMDoubleType();
    }

    { // zero constants
        ir_zerobool  = ir_bool(false);
        ir_zeroint   = ir_int(0);
        ir_zerofloat = ir_float(0.0);
    }

    { // printf
        LLVMT paramsT[1] = {irT_string};
        LLVMT T = LLVMFunctionType(irT_int, paramsT, 1, true);
        ir_printf_t = LLVMAddFunction(M, NAME_PRINTF, T);
    }
    
    { // malloc
        LLVMT paramsT[1] = {irT_int};
        LLVMT T = LLVMFunctionType(irT_pvoid, paramsT, 1, false);
        ir_malloc_t = LLVMAddFunction(M, NAME_MALLOC, T);
    }

    { // exit
        LLVMT paramsT[1] = {irT_int};
        LLVMT T = LLVMFunctionType(irT_void, paramsT, 1, false);
        ir_exit_t = LLVMAddFunction(M, NAME_EXIT, T);
    }
}

// ==================================================
//
//  Calls
//
// ==================================================

LLVMV ir_printf(LLVMB B, LLVMV* args, int n) {
    return LLVMBuildCall(B, ir_printf_t, args, n, LLVM_TMP_NONE);
}

LLVMV ir_malloc(LLVMB B, size_t size) {
    LLVMV args[1] = {ir_int(size)};
    return LLVMBuildCall(B, ir_malloc_t, args, 1, LLVM_TMP_NONE);
}

LLVMV ir_exit(LLVMB B) {
    LLVMV args[1] = {ir_int(1)};
    return LLVMBuildCall(B, ir_exit_t, args, 1, LLVM_TMP_NONE);
}

LLVMV ir_cmp(LLVMB B,
    LLVMIntPredicate iop,
    LLVMRealPredicate fop,
    Expression* lhs,
    Expression* rhs) {

    assert(lhs->type == rhs->type);

    if (lhs->type == __boolean) {
        return LLVMBuildICmp(B, iop, lhs->V, rhs->V, LLVM_TMP);
    } else if (lhs->type == __integer) {
        return LLVMBuildICmp(B, iop, lhs->V, rhs->V, LLVM_TMP);
    } else if (lhs->type == __float) {
        return LLVMBuildFCmp(B, fop, lhs->V, rhs->V, LLVM_TMP);
    } else {
        UNREACHABLE;
    }
}
