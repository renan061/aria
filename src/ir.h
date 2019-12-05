#if !defined(ir_h)
#define ir_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "ast.h"

// global string names
#define LLVM_GLOBAL_STRING "String"

// temporary names
#define LLVM_TMP        ("t")
#define LLVM_TMP_NONE   ("")
#define LLVM_TMP_PHI    ("phi")
#define LLVM_TMP_VMT    ("vmt")
#define LLVM_TMP_SELF   ("self")
#define LLVM_TMP_OBJ    ("obj")
#define LLVM_TMP_PROXY  ("proxy")
#define LLVM_TMP_OK     ("ok")
#define LLVM_TMP_RETURN ("ret")

// -----------------------------------------------------------------------------

#define DEFAULT_ADDRESS_SPACE 0

// types
extern     LLVMT    irT_pvoid     ;
extern     LLVMT    irT_void      ;
extern     LLVMT    irT_bool      ;
extern     LLVMT    irT_int       ;
extern     LLVMT    irT_float     ;
#define /* LLVMT */ irT_ptr(t)    (LLVMPointerType(t, DEFAULT_ADDRESS_SPACE))
#define /* LLVMT */ irT_string    (irT_pvoid)
#define /* LLVMT */ irT_array(t)  (irT_ptr(t))
#define /* LLVMT */ irT_interface (irT_pvoid)
#define /* LLVMT */ irT_monitor   (irT_pvoid)

// values
extern     LLVMV    ir_zerobool   ;
extern     LLVMV    ir_zeroint    ;
extern     LLVMV    ir_zerofloat  ;
extern     LLVMV    ir_zerostring ;
extern     LLVMV    ir_zeroptr    ;
#define /* LLVMV */ ir_bool(b)    (LLVMConstInt(irT_bool, b, false))
#define /* LLVMV */ ir_int(i)     (LLVMConstInt(irT_int, i, true))
#define /* LLVMv */ ir_float(f)   (LLVMConstReal(irT_float, f))

// -----------------------------------------------------------------------------

// native functions
extern LLVMV ir_printf(LLVMB, LLVMV*, int);
extern LLVMV ir_rand(LLVMB);
extern LLVMV ir_srand(LLVMB, LLVMV);

// auxiliary functions
extern LLVMV ir_malloc(LLVMB, size_t);
extern LLVMV ir_exit(LLVMB B);
extern LLVMV ir_cmp(LLVMB, LLVMIntPredicate, LLVMRealPredicate,
    Expression*, Expression*);

// -----------------------------------------------------------------------------

typedef struct IRState {
    LLVMM M;
    LLVMB B;

    bool main;        // if inside the main function
    bool initializer; // if inside an initializer

    // current function
    LLVMV function;

    // current basic block
    // must always be set after repositioning the builder
    // must always be set to NULL after adding a terminator instruction
    LLVMBB block; // TODO: rename to bb

    // definition of the structure currently being evaluated
    // must always be set before evaluating a structure's definitions
    // must always be set to NULL after evaluating a structure's definitions
    Definition* structure;

    // TODO: gambiarra
    LLVMV self;
} IRState;

extern IRState* irs_new(LLVMM, LLVMB);
extern void     irs_done(IRState*);
extern void     irs_destroy(IRState*);
extern void     irs_return(IRState*, LLVMV);
extern void     irsBB_start(IRState*, LLVMBB);
extern void     irsBB_end(IRState*);

// -----------------------------------------------------------------------------

// pthreads

// types
extern     LLVMT    irPTT_spawn;            // void* start_routine(void*)
#define /* LLVMT */ irPTT_mutex (irT_pvoid) // *pthread_mutex_t => *void
#define /* LLVMT */ irPTT_cond  (irT_pvoid) // *pthread_cond_t  => *void

// functions
extern void irPT_create(LLVMB, LLVMV fn, LLVMV arg);
extern void irPT_exit(LLVMB);
extern void irPT_mutex_init(LLVMB, LLVMV mutex);
extern void irPT_mutex_lock(LLVMB, LLVMV mutex);
extern void irPT_mutex_unlock(LLVMB, LLVMV mutex);
extern void irPT_cond_init(LLVMB, LLVMV cond);
extern void irPT_cond_wait(LLVMB, LLVMV cond, LLVMV mutex);
extern void irPT_cond_signal(LLVMB, LLVMV cond);
extern void irPT_cond_broadcast(LLVMB, LLVMV cond);

// -----------------------------------------------------------------------------

// must be called before using the module
extern void ir_setup(LLVMM);

#endif
