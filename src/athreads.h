#if !defined(athreads_h)
#define athreads_h

#include <llvm-c/Core.h>

#define LLVM_TYPE_POINTER_PTHREAD_MUTEX_T	LLVMT_PTR_VOID
#define LLVM_TYPE_POINTER_PTHREAD_COND_T	LLVMT_PTR_VOID

// ==================================================
//
//	Setup
//
// ==================================================

extern void ir_pthread_setup(LLVMModuleRef);

// The type pthread_create needs to receive as an argument
extern LLVMTypeRef ir_spawn_t;

// ==================================================
//
//	Calls to pthread.h using LLVM
//
// ==================================================

extern void ir_pthread_create(
	LLVMBuilderRef,
	LLVMValueRef function,
	LLVMValueRef argument
);

extern void ir_pthread_exit(LLVMBuilderRef);

extern void ir_pthread_mutex_init(LLVMBuilderRef, LLVMValueRef mutex);

extern void ir_pthread_mutex_lock(LLVMBuilderRef, LLVMValueRef mutex);

extern void ir_pthread_mutex_unlock(LLVMBuilderRef, LLVMValueRef mutex);

extern void ir_pthread_cond_init(LLVMBuilderRef, LLVMValueRef cond);

extern void ir_pthread_cond_wait(
	LLVMBuilderRef,
	LLVMValueRef cond,
	LLVMValueRef mutex
);

extern void ir_pthread_cond_signal(LLVMBuilderRef, LLVMValueRef cond);

extern void ir_pthread_cond_broadcast(LLVMBuilderRef, LLVMValueRef cond);

#endif
