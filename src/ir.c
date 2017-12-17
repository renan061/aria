#include <assert.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "alloc.h"
#include "ir.h"

#define NAME_PRINTF "printf"
#define NAME_MALLOC "malloc"

// Internal
LLVMValueRef
    ir_malloc_t = NULL,
    ir_printf_t = NULL
;

// ==================================================
//
//  IRState
//
// ==================================================

IRState* ir_state_new(LLVMModuleRef module, LLVMBuilderRef builder) {
    IRState* s;
    MALLOC(s, IRState);
    s->module = module;
    s->builder = builder;
    s->function = NULL;
    s->block = NULL;
    s->self = NULL;
    s->main = false;
    s->initializer = false;
    return s;
}

void ir_state_done(IRState* state) {
    char* error = NULL;
    LLVMVerifyModule(state->module, LLVMAbortProcessAction, &error);
    if (error) {
        LLVMDisposeMessage(error);
    }

    LLVMDisposeBuilder(state->builder);
}

void ir_state_free(IRState* state) {
    free(state);
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
            LLVMFunctionType(LLVM_TYPE_POINTER_VOID, paramtypes, 1, false)
        );
    }
}

// ==================================================
//
//  Calls
//
// ==================================================

LLVMValueRef ir_printf(LLVMBuilderRef builder, LLVMValueRef* args, int n) {
    return LLVMBuildCall(builder, ir_printf_t, args, n, LLVM_TEMPORARY_NONE);
}

LLVMValueRef ir_malloc(LLVMBuilderRef builder, size_t size) {
    LLVMValueRef args[1] = {LLVM_CONSTANT_INTEGER(size)};
    return LLVMBuildCall(builder, ir_malloc_t, args, 1, LLVM_TEMPORARY_NONE);
}

LLVMValueRef ir_cmp(LLVMBuilderRef builder,
    LLVMIntPredicate iop,
    LLVMRealPredicate fop,
    Expression* lhs,
    Expression* rhs) {

    assert(lhs->type == rhs->type);

    if (lhs->type == __boolean) {
        return LLVMBuildICmp(
            builder, iop, lhs->llvm_value, rhs->llvm_value, LLVM_TEMPORARY
        );
    } else if (lhs->type == __integer) {
        return LLVMBuildICmp(
            builder, iop, lhs->llvm_value, rhs->llvm_value, LLVM_TEMPORARY
        );
    } else if (lhs->type == __float) {
        return LLVMBuildICmp(
            builder, fop, lhs->llvm_value, rhs->llvm_value, LLVM_TEMPORARY
        );
    } else {
        UNREACHABLE;
    }
}
