#include <stdbool.h>
#include <stdlib.h>

#include <llvm-c/Core.h>

#include "athreads.h"
#include "ir.h"

#define LLVM_TYPE_POINTER_PTHREAD_T	LLVM_TYPE_POINTER_VOID

#define NAME_PTHREAD_CREATE			"pthread_create"
#define NAME_PTHREAD_EXIT			"pthread_exit"
#define NAME_PTHREAD_MUTEX_INIT		"pthread_mutex_init"
#define NAME_PTHREAD_MUTEX_LOCK		"pthread_mutex_lock"
#define NAME_PTHREAD_MUTEX_UNLOCK	"pthread_mutex_unlock"
#define NAME_PTHREAD_COND_INIT		"pthread_cond_init"
#define NAME_PTHREAD_COND_WAIT		"pthread_cond_wait"
#define NAME_PTHREAD_COND_SIGNAL	"pthread_cond_signal"
#define NAME_PTHREAD_COND_BROADCAST	"pthread_cond_broadcast"

// ==================================================
//
//	Declares
//
// ==================================================

LLVMTypeRef
	ir_spawn_t = NULL
;

LLVMValueRef
	ir_pthread_create_t			= NULL,
	ir_pthread_exit_t			= NULL,
	ir_pthread_mutex_init_t		= NULL,
	ir_pthread_mutex_lock_t		= NULL,
	ir_pthread_mutex_unlock_t	= NULL,
	ir_pthread_cond_init_t		= NULL,
	ir_pthread_cond_wait_t		= NULL,
	ir_pthread_cond_signal_t	= NULL,
	ir_pthread_cond_broadcast_t	= NULL
;

void ir_pthread_setup(LLVMModuleRef module) {
	{ // ir_spawn_t
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_VOID};
		ir_spawn_t = LLVMFunctionType(
			LLVM_TYPE_POINTER_VOID,
			paramtypes,
			1,
			false
		);
	}

	{ // pthread_create
		LLVMTypeRef paramtypes[4] = {
			LLVM_TYPE_POINTER_PTHREAD_T,
			LLVM_TYPE_POINTER_VOID,
			LLVM_TYPE_POINTER(ir_spawn_t),
			LLVM_TYPE_POINTER_VOID
		};
		ir_pthread_create_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_CREATE,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 4, false)
		);
	}

	{ // pthread_exit
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_VOID};
		ir_pthread_exit_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_EXIT,
			LLVMFunctionType(LLVM_TYPE_VOID, paramtypes, 1, false)
		);
	}
		
	{ // pthread_mutex_init
		LLVMTypeRef paramtypes[2] = {
			LLVM_TYPE_POINTER_PTHREAD_MUTEX_T,
			LLVM_TYPE_POINTER_VOID
		};
		ir_pthread_mutex_init_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_MUTEX_INIT,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 2, false)
		);
	}

	{ // pthread_mutex_lock
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_PTHREAD_MUTEX_T};
		ir_pthread_mutex_lock_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_MUTEX_LOCK,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 1, false)
		);
	}

	{ // pthread_mutex_unlock
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_PTHREAD_MUTEX_T};
		ir_pthread_mutex_unlock_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_MUTEX_UNLOCK,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 1, false)
		);
	}

	{ // pthread_cond_init
		LLVMTypeRef paramtypes[2] = {
			LLVM_TYPE_POINTER_PTHREAD_COND_T,
			LLVM_TYPE_POINTER_VOID
		};
		ir_pthread_cond_init_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_COND_INIT,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 2, false)
		);
	}

	{ // pthread_cond_wait
		LLVMTypeRef paramtypes[2] = {
			LLVM_TYPE_POINTER_PTHREAD_COND_T,
			LLVM_TYPE_POINTER_PTHREAD_MUTEX_T
		};
		ir_pthread_cond_wait_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_COND_WAIT,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 2, false)
		);
	}

	{ // pthread_cond_signal
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_PTHREAD_COND_T};
		ir_pthread_cond_signal_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_COND_SIGNAL,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 1, false)
		);
	}

	{ // pthread_cond_broadcast
		LLVMTypeRef paramtypes[1] = {LLVM_TYPE_POINTER_PTHREAD_COND_T};
		ir_pthread_cond_broadcast_t = LLVMAddFunction(
			module,
			NAME_PTHREAD_COND_BROADCAST,
			LLVMFunctionType(LLVM_TYPE_INTEGER, paramtypes, 1, false)
		);
	}
}

// ==================================================
//
//	Calls
//
// ==================================================

// ASK: Should the p_thread_t really be allocated with malloc?
void ir_pthread_create(
	LLVMBuilderRef builder,
	LLVMValueRef function,
	LLVMValueRef argument) {
	
	LLVMValueRef arguments[4] = {
		ir_malloc(builder, sizeof(pthread_t)),
		LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID),
		function,
		argument
	};
	LLVMBuildCall(
		builder, ir_pthread_create_t, arguments, 4, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_exit(LLVMBuilderRef builder) {
	LLVMValueRef args[1] = {LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)};
	LLVMBuildCall(builder, ir_pthread_exit_t, args, 1, LLVM_TEMPORARY_NONE);
}

void ir_pthread_mutex_init(LLVMBuilderRef builder, LLVMValueRef mutex) {
	LLVMValueRef args[2] = {
		mutex, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
	};
	LLVMBuildCall(
		builder, ir_pthread_mutex_init_t, args, 2, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_mutex_lock(LLVMBuilderRef builder, LLVMValueRef mutex) {
	LLVMValueRef args[1] = {mutex};
	LLVMBuildCall(
		builder, ir_pthread_mutex_lock_t, args, 1, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_mutex_unlock(LLVMBuilderRef builder, LLVMValueRef mutex) {
	LLVMValueRef args[1] = {mutex};
	LLVMBuildCall(
		builder, ir_pthread_mutex_unlock_t, args, 1, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_cond_init(LLVMBuilderRef builder, LLVMValueRef cond) {
	LLVMValueRef args[2] = {cond, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)};
	LLVMBuildCall(
		builder, ir_pthread_cond_init_t, args, 2, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_cond_wait(
	LLVMBuilderRef builder,
	LLVMValueRef cond,
	LLVMValueRef mutex) {

	LLVMValueRef args[2] = {cond, mutex};
	LLVMBuildCall(
		builder, ir_pthread_cond_wait_t, args, 2, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_cond_signal(LLVMBuilderRef builder, LLVMValueRef cond) {
	LLVMValueRef args[1] = {cond};
	LLVMBuildCall(
		builder, ir_pthread_cond_signal_t, args, 1, LLVM_TEMPORARY_NONE
	);
}

void ir_pthread_cond_broadcast(LLVMBuilderRef builder, LLVMValueRef cond) {
	LLVMValueRef args[1] = {cond};
	LLVMBuildCall(
		builder, ir_pthread_cond_broadcast_t, args, 1, LLVM_TEMPORARY_NONE
	);

}