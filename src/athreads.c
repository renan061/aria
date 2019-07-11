#include <stdbool.h>
#include <stdlib.h>

#include <llvm-c/Core.h>

#include "athreads.h"
#include "ir.h"

#define irT_ptr_PTHREAD_T irT_pvoid

#define NAME_PTHREAD_CREATE         "pthread_create"
#define NAME_PTHREAD_EXIT           "pthread_exit"
#define NAME_PTHREAD_MUTEX_INIT     "pthread_mutex_init"
#define NAME_PTHREAD_MUTEX_LOCK     "pthread_mutex_lock"
#define NAME_PTHREAD_MUTEX_UNLOCK   "pthread_mutex_unlock"
#define NAME_PTHREAD_COND_INIT      "pthread_cond_init"
#define NAME_PTHREAD_COND_WAIT      "pthread_cond_wait"
#define NAME_PTHREAD_COND_SIGNAL    "pthread_cond_signal"
#define NAME_PTHREAD_COND_BROADCAST "pthread_cond_broadcast"

// ==================================================
//
//  Declares
//
// ==================================================

LLVMT
    ir_spawn_t = NULL
;

LLVMV
    ir_pthread_create_t         = NULL,
    ir_pthread_exit_t           = NULL,
    ir_pthread_mutex_init_t     = NULL,
    ir_pthread_mutex_lock_t     = NULL,
    ir_pthread_mutex_unlock_t   = NULL,
    ir_pthread_cond_init_t      = NULL,
    ir_pthread_cond_wait_t      = NULL,
    ir_pthread_cond_signal_t    = NULL,
    ir_pthread_cond_broadcast_t = NULL
;

void ir_pthread_setup(LLVMM module) {
    { // ir_spawn_t
        LLVMT paramtypes[1] = {irT_pvoid};
        ir_spawn_t = LLVMFunctionType(
            irT_pvoid,
            paramtypes,
            1,
            false
        );
    }

    { // pthread_create
        LLVMT paramtypes[4] = {
            irT_ptr_PTHREAD_T,
            irT_pvoid,
            irT_ptr(ir_spawn_t),
            irT_pvoid
        };
        ir_pthread_create_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_CREATE,
            LLVMFunctionType(irT_int, paramtypes, 4, false)
        );
    }

    { // pthread_exit
        LLVMT paramtypes[1] = {irT_pvoid};
        ir_pthread_exit_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_EXIT,
            LLVMFunctionType(irT_void, paramtypes, 1, false)
        );
    }
        
    { // pthread_mutex_init
        LLVMT paramtypes[2] = {
            irT_ptr_PTHREAD_MUTEX_T,
            irT_pvoid
        };
        ir_pthread_mutex_init_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_MUTEX_INIT,
            LLVMFunctionType(irT_int, paramtypes, 2, false)
        );
    }

    { // pthread_mutex_lock
        LLVMT paramtypes[1] = {irT_ptr_PTHREAD_MUTEX_T};
        ir_pthread_mutex_lock_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_MUTEX_LOCK,
            LLVMFunctionType(irT_int, paramtypes, 1, false)
        );
    }

    { // pthread_mutex_unlock
        LLVMT paramtypes[1] = {irT_ptr_PTHREAD_MUTEX_T};
        ir_pthread_mutex_unlock_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_MUTEX_UNLOCK,
            LLVMFunctionType(irT_int, paramtypes, 1, false)
        );
    }

    { // pthread_cond_init
        LLVMT paramtypes[2] = {
            irT_ptr_PTHREAD_COND_T,
            irT_pvoid
        };
        ir_pthread_cond_init_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_COND_INIT,
            LLVMFunctionType(irT_int, paramtypes, 2, false)
        );
    }

    { // pthread_cond_wait
        LLVMT paramtypes[2] = {
            irT_ptr_PTHREAD_COND_T,
            irT_ptr_PTHREAD_MUTEX_T
        };
        ir_pthread_cond_wait_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_COND_WAIT,
            LLVMFunctionType(irT_int, paramtypes, 2, false)
        );
    }

    { // pthread_cond_signal
        LLVMT paramtypes[1] = {irT_ptr_PTHREAD_COND_T};
        ir_pthread_cond_signal_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_COND_SIGNAL,
            LLVMFunctionType(irT_int, paramtypes, 1, false)
        );
    }

    { // pthread_cond_broadcast
        LLVMT paramtypes[1] = {irT_ptr_PTHREAD_COND_T};
        ir_pthread_cond_broadcast_t = LLVMAddFunction(
            module,
            NAME_PTHREAD_COND_BROADCAST,
            LLVMFunctionType(irT_int, paramtypes, 1, false)
        );
    }
}

// ==================================================
//
//  Calls
//
// ==================================================

// ASK: Should the p_thread_t really be allocated with malloc?
void ir_pthread_create(LLVMB B, LLVMV fn, LLVMV args) {
    LLVMV arguments[] = {
        ir_malloc(B, sizeof(pthread_t)),
        LLVMConstPointerNull(irT_pvoid),
        fn,
        args
    };
    LLVMBuildCall(B, ir_pthread_create_t, arguments, 4, LLVM_TMP_NONE);
}

void ir_pthread_exit(LLVMB builder) {
    LLVMV args[] = {LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(builder, ir_pthread_exit_t, args, 1, LLVM_TMP_NONE);
}

void ir_pthread_mutex_init(LLVMB builder, LLVMV mutex) {
    LLVMV args[] = {mutex, LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(builder, ir_pthread_mutex_init_t, args, 2, LLVM_TMP_NONE);
}

void ir_pthread_mutex_lock(LLVMB builder, LLVMV mutex) {
    LLVMV args[] = {mutex};
    LLVMBuildCall(builder, ir_pthread_mutex_lock_t, args, 1, LLVM_TMP_NONE);
}

void ir_pthread_mutex_unlock(LLVMB builder, LLVMV mutex) {
    LLVMV args[] = {mutex};
    LLVMBuildCall(builder, ir_pthread_mutex_unlock_t, args, 1, LLVM_TMP_NONE);
}

void ir_pthread_cond_init(LLVMB builder, LLVMV cond) {
    LLVMV args[] = {cond, LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(builder, ir_pthread_cond_init_t, args, 2, LLVM_TMP_NONE);
}

void ir_pthread_cond_wait(LLVMB B, LLVMV cd, LLVMV mtx) {
    LLVMV args[] = {cd, mtx}; // {cond, mutex}
    LLVMBuildCall(B, ir_pthread_cond_wait_t, args, 2, LLVM_TMP_NONE);
}

void ir_pthread_cond_signal(LLVMB builder, LLVMV cond) {
    LLVMV args[] = {cond};
    LLVMBuildCall(builder, ir_pthread_cond_signal_t, args, 1, LLVM_TMP_NONE);
}

void ir_pthread_cond_broadcast(LLVMB builder, LLVMV cond) {
    LLVMV args[] = {cond};
    LLVMBuildCall(builder, ir_pthread_cond_broadcast_t, args, 1, LLVM_TMP_NONE);
}
