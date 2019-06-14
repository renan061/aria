#if !defined(athreads_h)
#define athreads_h

#include <llvm-c/Core.h>

#include "ast.h"

#define LLVMT_PTR_PTHREAD_MUTEX_T	LLVMT_PTR_VOID
#define LLVMT_PTR_PTHREAD_COND_T	LLVMT_PTR_VOID

extern void ir_pthread_setup(LLVMModuleRef);

// the type pthread_create needs to receive as an argument
extern LLVMT ir_spawn_t;

// calls to pthread.h using LLVM
extern void ir_pthread_create(LLVMB, LLVMV fn, LLVMV arg);
extern void ir_pthread_exit(LLVMB);
extern void ir_pthread_mutex_init(LLVMB, LLVMV mutex);
extern void ir_pthread_mutex_lock(LLVMB, LLVMV mutex);
extern void ir_pthread_mutex_unlock(LLVMB, LLVMV mutex);
extern void ir_pthread_cond_init(LLVMB, LLVMV cond);
extern void ir_pthread_cond_wait(LLVMB, LLVMV cond, LLVMV mutex);
extern void ir_pthread_cond_signal(LLVMB, LLVMV cond);
extern void ir_pthread_cond_broadcast(LLVMB, LLVMV cond);

#endif
