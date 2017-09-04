#include "allvm.h"

#define LLVM_TYPE_PTHREAD_T			LLVM_TYPE_POINTER_VOID
#define LLVM_TYPE_PTHREAD_MUTEX_T	LLVM_TYPE_POINTER_VOID
#define LLVM_TYPE_PTHREAD_COND_T	LLVM_TYPE_POINTER_VOID
#define LLVM_TYPE_CONDITION_QUEUE	LLVM_TYPE_PTHREAD_COND_T


	// ==================================================
	//
	//	TODO: Workshop area
	//
	// ==================================================

static LLVMValueRef workshop_malloc(IRState* s, size_t size) {
	// pthread_t*, not pthread_t ???

	LLVMValueRef
		fn = LLVMGetNamedFunction(s->module, NAME_MALLOC),
		args[1] = {/* size_t size */ LLVM_CONSTANT_INTEGER(size)}
	;
	return LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}

static void workshop(IRState* state) {
	// declaring void* malloc(size_t size);
	LLVMTypeRef
		param_types[1] = {/* size_t size */ LLVM_ARIA_TYPE_INTEGER},
		function_ty =
			LLVMFunctionType(LLVM_TYPE_POINTER_VOID, param_types, 1, false)
	;
	LLVMAddFunction(state->module, NAME_MALLOC, function_ty);
}

	// ==================================================
	//
	//	END Workshop
	//
	// ==================================================

// TODO: Docs
static LLVMTypeRef pt_type_spawn_function(void);
static LLVMValueRef pt_monitor_mutex(LLVMBuilderRef, LLVMValueRef);

// TODO: Docs
static void pt_declare_create(LLVMModuleRef);
static void pt_declare_exit(LLVMModuleRef);
static void pt_declare_mutex_init(LLVMModuleRef);
static void pt_declare_mutex_lock(LLVMModuleRef);
static void pt_declare_mutex_unlock(LLVMModuleRef);
static void pt_declare_cond_init(LLVMModuleRef);
static void pt_declare_cond_wait(LLVMModuleRef);
static void pt_declare_cond_signal(LLVMModuleRef);
static void pt_declare_cond_broadcast(LLVMModuleRef);

// TODO: Docs
static void pt_call_create(IRState*, LLVMValueRef, LLVMValueRef);
static void pt_call_exit(IRState*);
static void pt_call_mutex_init(IRState*, LLVMValueRef);
static void pt_call_mutex_lock(IRState*, LLVMValueRef);
static void pt_call_mutex_unlock(IRState*, LLVMValueRef);
static void pt_call_cond_init(IRState*, LLVMValueRef);
static void pt_call_cond_wait(IRState*, LLVMValueRef, LLVMValueRef);
static void pt_call_cond_signal(IRState*, LLVMValueRef);
static void pt_call_cond_broadcast(IRState*, LLVMValueRef);

// Returns the type the function pthread_create needs to receive as an argument
static LLVMTypeRef pt_type_spawn_function(void) {
	// void *(*start_routine)(void*)
	LLVMTypeRef parameters[1] = {LLVM_TYPE_POINTER_VOID};
	return LLVMFunctionType(
		/* ReturnType	*/ LLVM_TYPE_POINTER_VOID,
		/* ParamTypes	*/ parameters,
		/* ParamCount	*/ 1,
		/* IsVarArg		*/ false
	);
}

// Returns the mutex lock from a monitor
static LLVMValueRef pt_monitor_mutex(LLVMBuilderRef b, LLVMValueRef monitor) {
	return LLVMBuildLoad(
		b, LLVMBuildStructGEP(b, monitor, 0, LLVM_TEMPORARY), LLVM_TEMPORARY
	);
}

// int pthread_create(...)
static void pt_declare_create(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[4] = {
			// pthread_t *thread
			LLVM_TYPE_PTHREAD_T,
			// const pthread_attr_t *attr
			LLVM_TYPE_POINTER_VOID,
			// void *(*start_routine)(void*)
			LLVM_TYPE_POINTER(pt_type_spawn_function()),
			// void *arg
			LLVM_TYPE_POINTER_VOID
		},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 4, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_CREATE, function_ty);
}

// void pthread_exit(void *value_ptr)
static void pt_declare_exit(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_POINTER_VOID},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_VOID, param_types, 1, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_EXIT, function_ty);
}

// int pthread_mutex_init(...)
static void pt_declare_mutex_init(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[2] = {
			// pthread_mutex_t *mutex
			LLVM_TYPE_PTHREAD_MUTEX_T,
			// const pthread_mutexattr_t *attr
			LLVM_TYPE_POINTER_VOID
		},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 2, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_MUTEX_INIT, function_ty);
}

// int pthread_mutex_lock(pthread_mutex_t *mutex)
static void pt_declare_mutex_lock(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_PTHREAD_MUTEX_T},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 1, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_MUTEX_LOCK, function_ty);
}

// int pthread_mutex_unlock(pthread_mutex_t *mutex)
static void pt_declare_mutex_unlock(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_PTHREAD_MUTEX_T},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 1, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_MUTEX_UNLOCK, function_ty);
}

// int pthread_cond_init(pthread_cond_t *cond, const pthread_condattr_t *attr)
static void pt_declare_cond_init(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[2] = {LLVM_TYPE_PTHREAD_COND_T, LLVM_TYPE_POINTER_VOID},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 2, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_COND_INIT, function_ty);
}

// int pthread_cond_wait(pthread_cond_t *, pthread_mutex_t *mutex)
static void pt_declare_cond_wait(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[2] = {LLVM_TYPE_PTHREAD_COND_T, LLVM_TYPE_PTHREAD_MUTEX_T},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 2, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_COND_WAIT, function_ty);
}

// int pthread_cond_signal(pthread_cond_t *cond)
static void pt_declare_cond_signal(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_PTHREAD_COND_T},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 1, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_COND_SIGNAL, function_ty);
}

// int pthread_cond_broadcast(pthread_cond_t *cond)
static void pt_declare_cond_broadcast(LLVMModuleRef m) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_PTHREAD_COND_T},
		function_ty = LLVMFunctionType(LLVM_ARIA_TYPE_INTEGER, param_types, 1, false)
	;
	LLVMAddFunction(m, NAME_PTHREAD_COND_BROADCAST, function_ty);
}

static void pt_call_create(IRState* s, LLVMValueRef func, LLVMValueRef arg) {
	// TODO: Should the p_thread_t really be allocated with malloc?
	LLVMValueRef
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_CREATE),
		args[4] = {
			// pthread_t *thread
			// WORK: New Malloc
			workshop_malloc(s, sizeof(pthread_t)),
			// const pthread_attr_t *attr
			LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID),
			// void *(*start_routine)(void*)
			func,
			// void *arg
			arg
		}
	;
	LLVMBuildCall(s->builder, fn, args, 4, LLVM_TEMPORARY_NONE);
}


static void pt_call_exit(IRState* s) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_EXIT),
		args[1] = {LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)}
	;
	LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}

static void pt_call_mutex_init(IRState* s, LLVMValueRef lock) {
	LLVMValueRef 		
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_MUTEX_INIT),
		args[2] = {
			// pthread_mutex_t *mutex,
			lock,
			// const pthread_mutexattr_t *attr
			LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
		}
	;
	LLVMBuildCall(s->builder, fn, args, 2, LLVM_TEMPORARY_NONE);
}

static void pt_call_mutex_lock(IRState* s, LLVMValueRef lock) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_MUTEX_LOCK),
		args[1] = {lock}
	;
	LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}

static void pt_call_mutex_unlock(IRState* s, LLVMValueRef lock) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_MUTEX_UNLOCK),
		args[1] = {lock}
	;
	LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}

static void pt_call_cond_init(IRState* s, LLVMValueRef cond) {
	LLVMValueRef 		
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_COND_INIT),
		args[2] = {cond, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)}
	;
	LLVMBuildCall(s->builder, fn, args, 2, LLVM_TEMPORARY_NONE);
}

static void pt_call_cond_wait(IRState* s, LLVMValueRef cond, LLVMValueRef mutex) {

	LLVMValueRef 		
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_COND_WAIT),
		args[2] = {cond, mutex}
	;
	LLVMBuildCall(s->builder, fn, args, 2, LLVM_TEMPORARY_NONE);
}

static void pt_call_cond_signal(IRState* s, LLVMValueRef cond) {
	LLVMValueRef 		
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_COND_SIGNAL),
		args[1] = {cond}
	;
	LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}

static void pt_call_cond_broadcast(IRState* s, LLVMValueRef cond) {
	LLVMValueRef 		
		fn = LLVMGetNamedFunction(s->module, NAME_PTHREAD_COND_BROADCAST),
		args[1] = {cond}
	;
	LLVMBuildCall(s->builder, fn, args, 1, LLVM_TEMPORARY_NONE);
}
