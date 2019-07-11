#if !defined(ir_h)
#define ir_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "ast.h"
#include "athreads.h"

// global names
#define LLVM_GLOBAL_STRING "String"

// temporary names
#define LLVM_TMP                    "t"
#define LLVM_TMP_NONE               ""
#define LLVM_TEMPORARY_MONITOR_LOCK LLVM_TMP "monitor_lock"
#define LLVM_TEMPORARY_PHI          LLVM_TMP "phi"
#define LLVM_TMP_VMT                "vmt"
#define LLVM_TMP_SELF               "self"
#define LLVM_TMP_OBJ                "obj"
#define LLVM_TMP_PROXY              "proxy"
#define LLVM_TMP_OK                 "ok"
#define LLVM_TMP_RETURN             "ret"

// ==================================================
//
//  IRState
//
// ==================================================

typedef struct IRState {
    LLVMM M;
    LLVMB B;

    // current function
    LLVMV function;

    // current basic block
    // must always be set after repositioning the builder
    // must always be set to NULL after adding a terminator instruction
    LLVMBB block;

    // definition of the structure currently being evaluated
    // must always be set before evaluating a structure's definitions
    // must always be set to NULL after evaluating a structure's definitions
    Definition* structure;

    // TODO: gambiarra
    LLVMV self;

    // if inside the main function
    bool main;

    // if inside an initializer
    bool initializer;
} IRState;

extern IRState* ir_state_new(LLVMM, LLVMB);
extern void ir_state_done(IRState*);
extern void ir_state_free(IRState*);

// ==================================================
//
//  Functions
//
// ==================================================

extern void ir_setup(LLVMM);

extern LLVMV ir_printf(LLVMB, LLVMV*, int);
extern LLVMV ir_malloc(LLVMB, size_t);
extern LLVMV ir_exit(LLVMB B);

extern LLVMV ir_cmp(LLVMB, LLVMIntPredicate, LLVMRealPredicate,
    Expression*, Expression*);

// types
extern LLVMT irT_pvoid;
extern LLVMT irT_void;
extern LLVMT irT_bool;
extern LLVMT irT_int;
extern LLVMT irT_float;
#define irT_ptr(t)    (LLVMPointerType(t, 0))
#define irT_string    (irT_pvoid)
#define irT_array(t)  (irT_ptr(t))
#define irT_interface (irT_pvoid)
#define irT_monitor   (irT_pvoid)
#define irT_cq        (irT_pvoid)

// constants
extern LLVMV ir_zerobool;
extern LLVMV ir_zeroint;
extern LLVMV ir_zerofloat;
#define ir_bool(b)  (LLVMConstInt(irT_bool, b, false))
#define ir_int(i)   (LLVMConstInt(irT_int, i, true))
#define ir_float(f) (LLVMConstReal(irT_float, f))

#endif
