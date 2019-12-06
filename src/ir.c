#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "alloc.h"
#include "ir.h"
#include "macros.h"

// -----------------------------------------------------------------------------

LLVMT irT_pvoid = NULL;
LLVMT irT_void  = NULL;
LLVMT irT_bool  = NULL;
LLVMT irT_int   = NULL;
LLVMT irT_float = NULL;

LLVMV ir_zerobool   = NULL;
LLVMV ir_zeroint    = NULL;
LLVMV ir_zerofloat  = NULL;
LLVMV ir_zerostring = NULL;
LLVMV ir_zeroptr    = NULL;

// TODO: duplicated => also in backend.c
static Type* __boolean;
static Type* __integer;
static Type* __float;

static const char* NAME_PRINTF          = "printf";
static const char* NAME_RAND            = "rand";
static const char* NAME_SRAND           = "srand";
static const char* NAME_STRUCT_TIMESPEC = "struct_timespec";
static const char* NAME_CLOCK_GETTIME   = "clock_gettime";

static const char* NAME_MALLOC = "malloc";
static const char* NAME_EXIT   = "exit";

static LLVMV irV_printf          = NULL;
static LLVMV irV_rand            = NULL;
static LLVMV irV_srand           = NULL;
static LLVMT irT_struct_timespec = NULL;
static LLVMV irV_clock_gettime   = NULL;

static LLVMV irV_malloc = NULL;
static LLVMV irV_exit   = NULL;

static void irPT_setup(LLVMM);

void ir_setup(LLVMM M) {
    LLVMC C = LLVMGetGlobalContext();

    // aria primitive types
    __boolean = ast_type_boolean();
    __integer = ast_type_integer();
    __float = ast_type_float();

    // ir types
    irT_pvoid = irT_ptr(LLVMInt8Type());
    irT_void  = LLVMVoidType();
    irT_bool  = LLVMIntType(1);
    irT_int   = LLVMInt32Type();
    irT_float = LLVMDoubleType();

    // ir zero values
    ir_zerobool   = ir_bool(false);
    ir_zeroint    = ir_int(0);
    ir_zerofloat  = ir_float(0.0);
    ir_zeroptr    = LLVMConstPointerNull(irT_pvoid);
    ir_zerostring = LLVMAddGlobal(M, LLVMInt8Type(), "zerostring");

    LLVMSetInitializer(ir_zerostring, LLVMConstInt(LLVMInt8Type(), '\0', true));

    LLVMT T;

    { // printf
        T = LLVMFunctionType(irT_int, &irT_string, 1, true);
        irV_printf = LLVMAddFunction(M, NAME_PRINTF, T);
    }

    { // rand
        T = LLVMFunctionType(irT_int, NULL, 0, false);
        irV_rand = LLVMAddFunction(M, NAME_RAND, T);
    }

    { // srand
        T = LLVMFunctionType(irT_void, &irT_int, 1, false);
        irV_srand = LLVMAddFunction(M, NAME_SRAND, T);
    }

    { // clock_gettime
        LLVMT i64 = LLVMIntType(64);
        LLVMT i32 = LLVMIntType(32);

        irT_struct_timespec = LLVMStructCreateNamed(C, NAME_STRUCT_TIMESPEC);
        LLVMT ETs[] = {i64, i64};
        LLVMStructSetBody(irT_struct_timespec, ETs, 2, false);

        LLVMT PTs[] = {irT_int, irT_ptr(irT_struct_timespec)};
        T = LLVMFunctionType(i32, PTs, 2, false);
        irV_clock_gettime = LLVMAddFunction(M, NAME_CLOCK_GETTIME, T);
    }

    { // malloc
        T = LLVMFunctionType(irT_pvoid, &irT_int, 1, false);
        irV_malloc = LLVMAddFunction(M, NAME_MALLOC, T);
    }

    { // exit
        T = LLVMFunctionType(irT_void, &irT_int, 1, false);
        irV_exit = LLVMAddFunction(M, NAME_EXIT, T);
    }

    irPT_setup(M);
}

// -----------------------------------------------------------------------------

LLVMV ir_printf(LLVMB B, LLVMV* args, int n) {
    return LLVMBuildCall(B, irV_printf, args, n, LLVM_TMP_NONE);
}

LLVMV ir_rand(LLVMB B) {
    return LLVMBuildCall(B, irV_printf, NULL, 0, LLVM_TMP_NONE);
}

LLVMV ir_srand(LLVMB B, LLVMV arg) {
    return LLVMBuildCall(B, irV_srand, &arg, 1, LLVM_TMP_NONE);
}

LLVMV ir_getTime(LLVMB B) {
    LLVMV id = LLVMConstInt(irT_int, CLOCK_REALTIME, false);
    LLVMV ts = LLVMBuildAlloca(B, irT_struct_timespec, LLVM_TMP);
    LLVMV args[] = {id, ts};
    LLVMBuildCall(B, irV_clock_gettime, args, 2, LLVM_TMP_NONE);

    LLVMV sec = LLVMBuildStructGEP(B, ts, 0, LLVM_TMP);
    sec = LLVMBuildLoad(B, sec, LLVM_TMP);
    LLVMV nsec = LLVMBuildStructGEP(B, ts, 1, LLVM_TMP);
    nsec = LLVMBuildLoad(B, nsec, LLVM_TMP);
    LLVMV nano = LLVMConstReal(irT_float, 1e9);
    nsec = LLVMBuildSIToFP(B, nsec, irT_float, LLVM_TMP);
    nsec = LLVMBuildFDiv(B, nsec, nano, LLVM_TMP);
    sec = LLVMBuildSIToFP(B, sec, irT_float, LLVM_TMP);
    sec = LLVMBuildFAdd(B, sec, nsec, LLVM_TMP);
    return sec;
}

LLVMV ir_malloc(LLVMB B, size_t size) {
    LLVMV args[] = {ir_int(size)};
    return LLVMBuildCall(B, irV_malloc, args, 1, LLVM_TMP);
}

LLVMV ir_exit(LLVMB B) {
    LLVMV args[] = {ir_int(1)};
    return LLVMBuildCall(B, irV_exit, args, 1, LLVM_TMP_NONE);
}

LLVMV ir_cmp(LLVMB B, LLVMIntPredicate iop, LLVMRealPredicate fop,
    Expression* l, Expression* r) {

    assert(l->type == r->type);
    if (l->type == __boolean) {
        return LLVMBuildICmp(B, iop, l->V, r->V, LLVM_TMP);
    } else if (l->type == __integer) {
        return LLVMBuildICmp(B, iop, l->V, r->V, LLVM_TMP);
    } else if (l->type == __float) {
        return LLVMBuildFCmp(B, fop, l->V, r->V, LLVM_TMP);
    } else {
        UNREACHABLE;
    }
}

// ==================================================
//
//  irs
//
// ==================================================

IRState* irs_new(LLVMM M, LLVMB B) {
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

void irs_done(IRState* irs) {
    char* err = NULL;
    // TODO: LLVMAbortProcessAction
    bool ok = !LLVMVerifyModule(irs->M, LLVMReturnStatusAction, &err);
    if (!ok) {
        printf("VerifyModule error...\n");
        LLVMDumpModule(irs->M);
        printf("-----\n%s\n", err);
        exit(1);
    }
    LLVMDisposeBuilder(irs->B);
}

void irs_destroy(IRState* irs) {
    free(irs);
}

void irs_return(IRState* irs, LLVMV V) {
    if (irs->main) {
        irPT_exit(irs->B);
        goto RETURN_VOID;
    }
    if (irs->initializer && !V) {
        V = LLVMBuildBitCast(irs->B, irs->self, irT_pvoid, LLVM_TMP);
        goto RETURN_VALUE;
    }
    if (V) RETURN_VALUE: {
        LLVMBuildRet(irs->B, V);
    } else RETURN_VOID:  {
        LLVMBuildRetVoid(irs->B);
    }
    irsBB_end(irs);
}

void irsBB_start(IRState* irs, LLVMBB bb) {
    assert(!irs->block);
    LLVMPositionBuilderAtEnd(irs->B, bb);
    irs->block = bb;
}

void irsBB_end(IRState* irs) {
    assert(irs->block);
    irs->block = NULL;
}

// ==================================================
//
//  pthreads
//
// ==================================================

LLVMT irPTT_spawn = NULL;

#define irPTT_pthread (irT_pvoid) // *pthread_t => *void

static const char* NAME_PT_CREATE         = "pthread_create";
static const char* NAME_PT_EXIT           = "pthread_exit";
static const char* NAME_PT_MUTEX_INIT     = "pthread_mutex_init";
static const char* NAME_PT_MUTEX_LOCK     = "pthread_mutex_lock";
static const char* NAME_PT_MUTEX_UNLOCK   = "pthread_mutex_unlock";
static const char* NAME_PT_COND_INIT      = "pthread_cond_init";
static const char* NAME_PT_COND_WAIT      = "pthread_cond_wait";
static const char* NAME_PT_COND_SIGNAL    = "pthread_cond_signal";
static const char* NAME_PT_COND_BROADCAST = "pthread_cond_broadcast";

static LLVMV irPTT_create         = NULL;
static LLVMV irPTT_exit           = NULL;
static LLVMV irPTT_mutex_init     = NULL;
static LLVMV irPTT_mutex_lock     = NULL;
static LLVMV irPTT_mutex_unlock   = NULL;
static LLVMV irPTT_cond_init      = NULL;
static LLVMV irPTT_cond_wait      = NULL;
static LLVMV irPTT_cond_signal    = NULL;
static LLVMV irPTT_cond_broadcast = NULL;

static void irPT_setup(LLVMM M) {
    { // irPTT_spawn
        LLVMT Ts[] = {irT_pvoid};
        irPTT_spawn = LLVMFunctionType(irT_pvoid, Ts, 1, false);
    }

    { // pthread_create
        LLVMT ptr_spawn = irT_ptr(irPTT_spawn);
        LLVMT Ts[] = {irPTT_pthread, irT_pvoid, ptr_spawn, irT_pvoid};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 4, false);
        irPTT_create = LLVMAddFunction(M, NAME_PT_CREATE, T);
    }

    { // pthread_exit
        LLVMT Ts[] = {irT_pvoid};
        LLVMT T = LLVMFunctionType(irT_void, Ts, 1, false);
        irPTT_exit = LLVMAddFunction(M, NAME_PT_EXIT, T);
    }

    { // pthread_mutex_init
        LLVMT Ts[] = {irPTT_mutex, irT_pvoid};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 2, false);
        irPTT_mutex_init = LLVMAddFunction(M, NAME_PT_MUTEX_INIT, T);
    }

    { // pthread_mutex_lock
        LLVMT Ts[] = {irPTT_mutex};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 1, false);
        irPTT_mutex_lock = LLVMAddFunction(M, NAME_PT_MUTEX_LOCK, T);
    }

    { // pthread_mutex_unlock
        LLVMT Ts[] = {irPTT_mutex};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 1, false);
        irPTT_mutex_unlock = LLVMAddFunction(M, NAME_PT_MUTEX_UNLOCK, T);
    }

    { // pthread_cond_init
        LLVMT Ts[] = {irPTT_cond, irT_pvoid};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 2, false);
        irPTT_cond_init = LLVMAddFunction(M, NAME_PT_COND_INIT, T);
    }

    { // pthread_cond_wait
        LLVMT Ts[] = {irPTT_cond, irPTT_mutex};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 2, false);
        irPTT_cond_wait = LLVMAddFunction(M, NAME_PT_COND_WAIT, T);
    }

    { // pthread_cond_signal
        LLVMT Ts[] = {irPTT_cond};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 1, false);
        irPTT_cond_signal = LLVMAddFunction(M, NAME_PT_COND_SIGNAL, T);
    }

    { // pthread_cond_broadcast
        LLVMT Ts[] = {irPTT_cond};
        LLVMT T = LLVMFunctionType(irT_int, Ts, 1, false);
        irPTT_cond_broadcast = LLVMAddFunction(M, NAME_PT_COND_BROADCAST, T);
    }
}

#undef irPTT_pthread

void irPT_create(LLVMB B, LLVMV start_routine, LLVMV arg) {
    // ASK: should the p_thread_t really be allocated with malloc?
    LLVMV thread = ir_malloc(B, sizeof(pthread_t));
    LLVMV attr = LLVMConstPointerNull(irT_pvoid);
    LLVMV arguments[] = {thread, attr, start_routine, arg};
    LLVMBuildCall(B, irPTT_create, arguments, 4, LLVM_TMP);
}

void irPT_exit(LLVMB B) {
    LLVMV args[] = {LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(B, irPTT_exit, args, 1, LLVM_TMP_NONE);
}

void irPT_mutex_init(LLVMB B, LLVMV mutex) {
    LLVMV args[] = {mutex, LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(B, irPTT_mutex_init, args, 2, LLVM_TMP);
}

void irPT_mutex_lock(LLVMB B, LLVMV mutex) {
    LLVMV args[] = {mutex};
    LLVMBuildCall(B, irPTT_mutex_lock, args, 1, LLVM_TMP);
}

void irPT_mutex_unlock(LLVMB B, LLVMV mutex) {
    LLVMV args[] = {mutex};
    LLVMBuildCall(B, irPTT_mutex_unlock, args, 1, LLVM_TMP);
}

void irPT_cond_init(LLVMB B, LLVMV cond) {
    LLVMV args[] = {cond, LLVMConstPointerNull(irT_pvoid)};
    LLVMBuildCall(B, irPTT_cond_init, args, 2, LLVM_TMP);
}

void irPT_cond_wait(LLVMB B, LLVMV cond, LLVMV mutex) {
    LLVMV args[] = {cond, mutex};
    LLVMBuildCall(B, irPTT_cond_wait, args, 2, LLVM_TMP);
}

void irPT_cond_signal(LLVMB B, LLVMV cond) {
    LLVMV args[] = {cond};
    LLVMBuildCall(B, irPTT_cond_signal, args, 1, LLVM_TMP);
}

void irPT_cond_broadcast(LLVMB B, LLVMV cond) {
    LLVMV args[] = {cond};
    LLVMBuildCall(B, irPTT_cond_broadcast, args, 1, LLVM_TMP);
}
