#if !defined(ir_h)
#define ir_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "ast.h"
#include "athreads.h"

#define LLVM_DEFAULT_ADDRESS_SPACE 0

// Temporary Names
#define LLVM_TEMPORARY              "_t_"
#define LLVM_TEMPORARY_NONE         ""
#define LLVM_TEMPORARY_MONITOR_LOCK LLVM_TEMPORARY "monitor_lock_"
#define LLVM_TEMPORARY_PHI          LLVM_TEMPORARY "phi_"

// Types
#define LLVM_TYPE_POINTER(t)    LLVMPointerType(t, LLVM_DEFAULT_ADDRESS_SPACE)
#define LLVM_TYPE_POINTER_VOID  LLVM_TYPE_POINTER(LLVMInt8Type())
#define LLVM_TYPE_VOID          LLVMVoidType()
#define LLVM_TYPE_BOOLEAN       LLVMIntType(1)
#define LLVM_TYPE_INTEGER       LLVMInt32Type()
#define LLVM_TYPE_DOUBLE        LLVMDoubleType()

// Aria Types
#define LLVM_ARIA_TYPE_VOID             LLVM_TYPE_VOID
#define LLVM_ARIA_TYPE_BOOLEAN          LLVM_TYPE_BOOLEAN
#define LLVM_ARIA_TYPE_INTEGER          LLVM_TYPE_INTEGER
#define LLVM_ARIA_TYPE_FLOAT            LLVM_TYPE_DOUBLE
#define LLVM_ARIA_TYPE_STRING           LLVM_TYPE_POINTER(LLVMInt8Type())
#define LLVM_ARIA_TYPE_ARRAY(t)         LLVM_TYPE_POINTER(t);
#define LLVM_ARIA_TYPE_MONITOR(s)       LLVM_TYPE_POINTER(s)
#define LLVM_ARIA_TYPE_CONDITION_QUEUE  LLVM_TYPE_POINTER_PTHREAD_COND_T

// ASK: Should SignExtend?
// TODO: Not necessarily LLVM_ARIA_TYPES

// Constants
#define LLVM_CONSTANT_BOOLEAN(b)    LLVMConstInt(LLVM_ARIA_TYPE_BOOLEAN, b, 0)
#define LLVM_CONSTANT_INTEGER(i)    LLVMConstInt(LLVM_ARIA_TYPE_INTEGER, i, 0)
#define LLVM_CONSTANT_FLOAT(f)      LLVMConstReal(LLVM_ARIA_TYPE_FLOAT, f)
#define LLVM_CONSTANT_TRUE          LLVM_CONSTANT_BOOLEAN(1)
#define LLVM_CONSTANT_FALSE         LLVM_CONSTANT_BOOLEAN(0)

// ==================================================
//
//  IRState
//
// ==================================================

typedef struct IRState {
    LLVMModuleRef module;
    LLVMBuilderRef builder;

    // current function
    LLVMValueRef function;

    // current basic block
    // must always be set after repositioning the builder
    // must always be set to NULL after adding a terminator instruction
    LLVMBasicBlockRef block;

    // TODO: gambiarra
    LLVMValueRef self;

    // if inside the main function
    bool main;

    // if inside an initializer
    bool initializer;
} IRState;

extern IRState* ir_state_new(LLVMModuleRef, LLVMBuilderRef);
extern void ir_state_done(IRState*);
extern void ir_state_free(IRState*);

// ==================================================
//
//  Functions
//
// ==================================================

extern void ir_setup(LLVMModuleRef);

extern LLVMValueRef ir_printf(LLVMBuilderRef, LLVMValueRef*, int);
extern LLVMValueRef ir_malloc(LLVMBuilderRef, size_t);

extern LLVMValueRef ir_cmp(LLVMBuilderRef, LLVMIntPredicate, LLVMRealPredicate,
    Expression*, Expression*);

#endif
