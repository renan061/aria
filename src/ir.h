#if !defined(ir_h)
#define ir_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "ast.h"
#include "athreads.h"

#define LLVM_DEFAULT_ADDRESS_SPACE 0

// global names
#define LLVM_GLOBAL_STRING "_global_string"

// temporary names
#define LLVM_TMP                    "t"
#define LLVM_TMP_NONE               ""
#define LLVM_TEMPORARY_MONITOR_LOCK LLVM_TMP "monitor_lock"
#define LLVM_TEMPORARY_PHI          LLVM_TMP "phi"
#define LLVM_TMP_VMT                "vmt"
#define LLVM_TMP_SELF               "self"
#define LLVM_TMP_PROXY              "proxy"
#define LLVM_TMP_OK                 "ok"
#define LLVM_TMP_RETURN             "ret"

// types
#define LLVMT_PTR(t)        (LLVMPointerType(t, LLVM_DEFAULT_ADDRESS_SPACE))
#define LLVMT_PTR_VOID      (LLVMT_PTR(LLVMInt8Type()))
#define LLVM_TYPE_VOID      (LLVMVoidType())
#define LLVMT_BOOLEAN       (LLVMIntType(1))
#define LLVM_TYPE_INTEGER   (LLVMInt32Type())
#define LLVM_TYPE_DOUBLE    (LLVMDoubleType())

// aria types
#define LLVM_ARIA_TYPE_VOID             LLVM_TYPE_VOID
#define LLVM_ARIA_TYPE_BOOLEAN          LLVMT_BOOLEAN
#define LLVM_ARIA_TYPE_INTEGER          LLVM_TYPE_INTEGER
#define LLVM_ARIA_TYPE_FLOAT            LLVM_TYPE_DOUBLE
#define LLVM_ARIA_TYPE_STRING           LLVMT_PTR(LLVMInt8Type())
#define LLVM_ARIA_TYPE_ARRAY(t)         LLVMT_PTR(t)
#define LLVM_ARIA_TYPE_INTERFACE        LLVMT_PTR_VOID
#define LLVM_ARIA_TYPE_MONITOR          LLVMT_PTR_VOID
#define LLVM_ARIA_TYPE_CONDITION_QUEUE  LLVM_TYPE_POINTER_PTHREAD_COND_T

#define LLVMT_AOBJ                      LLVMT_PTR_VOID

// ASK: should SignExtend?
// TODO: not necessarily LLVM_ARIA_TYPES

// constants
#define LLVM_CONSTANT_BOOLEAN(b)    LLVMConstInt(LLVM_ARIA_TYPE_BOOLEAN, b, 0)
#define LLVM_CONST_INT(i)    LLVMConstInt(LLVM_ARIA_TYPE_INTEGER, i, 0)
#define LLVM_CONSTANT_FLOAT(f)      LLVMConstReal(LLVM_ARIA_TYPE_FLOAT, f)
#define LLVM_CONSTANT_TRUE          LLVM_CONSTANT_BOOLEAN(1)
#define LLVM_CONSTANT_FALSE         LLVM_CONSTANT_BOOLEAN(0)

// ==================================================
//
//  IRState
//
// ==================================================

typedef struct IRState {
    LLVMModuleRef M;
    LLVMBuilderRef B;

    // current function
    LLVMValueRef function;

    // current basic block
    // must always be set after repositioning the builder
    // must always be set to NULL after adding a terminator instruction
    LLVMBasicBlockRef block;

    // definition of the structure currently being evaluated
    // must always be set before evaluating a structure's definitions
    // must always be set to NULL after evaluating a structure's definitions
    Definition* structure;

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
extern LLVMValueRef ir_exit(LLVMBuilderRef B);

extern LLVMValueRef ir_cmp(LLVMBuilderRef, LLVMIntPredicate, LLVMRealPredicate,
    Expression*, Expression*);

#endif
