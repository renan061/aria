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

// Internal
static LLVMValueRef
    ir_malloc_t = NULL,
    ir_printf_t = NULL,
    ir_exit_t   = NULL
;

// ==================================================
//
//  IRState
//
// ==================================================

IRState* ir_state_new(LLVMModuleRef M, LLVMBuilderRef B) {
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

// TODO: Duplicated -> Also in backend.c
static Type* __boolean;
static Type* __integer;
static Type* __float;

void ir_setup(LLVMModuleRef module) {
    { // primitive types
        __boolean = ast_type_boolean();
        __integer = ast_type_integer();
        __float = ast_type_float();
    }

    { // printf
        LLVMTypeRef paramtypes[1] = {LLVM_ARIA_TYPE_STRING};
        ir_printf_t = LLVMAddFunction(
            module,
            NAME_PRINTF,
            LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, paramtypes, 1, true)
        );
    }
    
    { // malloc
        LLVMTypeRef paramtypes[1] = {LLVM_TYPE_INTEGER};
        ir_malloc_t = LLVMAddFunction(
            module,
            NAME_MALLOC,
            LLVMFunctionType(LLVMT_PTR_VOID, paramtypes, 1, false)
        );
    }

    { // exit
        LLVMTypeRef paramtypes[1] = {LLVM_TYPE_INTEGER};
        ir_exit_t = LLVMAddFunction(
            module,
            NAME_EXIT,
            LLVMFunctionType(LLVMT_VOID, paramtypes, 1, false)
        );
    }
}

// ==================================================
//
//  Calls
//
// ==================================================

LLVMValueRef ir_printf(LLVMBuilderRef B, LLVMValueRef* args, int n) {
    return LLVMBuildCall(B, ir_printf_t, args, n, LLVM_TMP_NONE);
}

LLVMValueRef ir_malloc(LLVMBuilderRef B, size_t size) {
    LLVMValueRef args[1] = {LLVM_CONST_INT(size)};
    return LLVMBuildCall(B, ir_malloc_t, args, 1, LLVM_TMP_NONE);
}

LLVMValueRef ir_exit(LLVMBuilderRef B) {
    LLVMValueRef args[1] = {LLVM_CONST_INT(1)};
    return LLVMBuildCall(B, ir_exit_t, args, 1, LLVM_TMP_NONE);
}

LLVMValueRef ir_cmp(LLVMBuilderRef B,
    LLVMIntPredicate iop,
    LLVMRealPredicate fop,
    Expression* lhs,
    Expression* rhs) {

    assert(lhs->type == rhs->type);

    if (lhs->type == __boolean) {
        return LLVMBuildICmp(
            B, iop, lhs->V, rhs->V, LLVM_TMP
        );
    } else if (lhs->type == __integer) {
        return LLVMBuildICmp(
            B, iop, lhs->V, rhs->V, LLVM_TMP
        );
    } else if (lhs->type == __float) {
        return LLVMBuildFCmp(
            B, fop, lhs->V, rhs->V, LLVM_TMP
        );
    } else {
        UNREACHABLE;
    }
}
