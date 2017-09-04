#if !defined(allvm_h)
#define allvm_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#define LLVM_DEFAULT_ADDRESS_SPACE 0

// ==================================================
//
//	Types
//
// ==================================================

#define LLVM_TYPE_POINTER(t)	LLVMPointerType(t, LLVM_DEFAULT_ADDRESS_SPACE)
#define LLVM_TYPE_MONITOR(s)	LLVM_TYPE_POINTER(s)
#define LLVM_TYPE_POINTER_VOID	LLVM_TYPE_POINTER(LLVMInt8Type())

#define LLVM_ARIA_TYPE_VOID			LLVMVoidType()
#define LLVM_ARIA_TYPE_BOOLEAN		LLVMIntType(1)
#define LLVM_ARIA_TYPE_INTEGER		LLVMInt32Type()
#define LLVM_ARIA_TYPE_FLOAT		LLVMDoubleType()
#define LLVM_ARIA_TYPE_STRING		LLVM_TYPE_POINTER(LLVMInt8Type())

// ==================================================
//
//	Constants
//
// ==================================================

// ASK: Should SignExtend?

// TODO: Not necessarily LLVM_ARIA_TYPES

#define LLVM_CONSTANT_BOOLEAN(b)	LLVMConstInt(LLVM_ARIA_TYPE_BOOLEAN, b, 0)
#define LLVM_CONSTANT_INTEGER(i)	LLVMConstInt(LLVM_ARIA_TYPE_INTEGER, i, 0)
#define LLVM_CONSTANT_FLOAT(f)		LLVMConstReal(LLVM_ARIA_TYPE_FLOAT, f)
#define LLVM_CONSTANT_TRUE			LLVM_CONSTANT_BOOLEAN(1)
#define LLVM_CONSTANT_FALSE 		LLVM_CONSTANT_BOOLEAN(0)

// ==================================================
//
//	IRState
//
// ==================================================

typedef struct IRState {
	LLVMModuleRef module;
	LLVMBuilderRef builder;
    LLVMValueRef function; // current function

    /*
     * The current basic block.
     * Must always set this after repositioning the builder.
     * Must always set this to NULL after adding a terminator instruction.
     */
    LLVMBasicBlockRef block;

    LLVMValueRef self; // TODO: Gambiarra

    bool main;			// if inside the main function
    bool initializer;	// if inside an initializer
} IRState;

extern IRState* allvm_irstate_new(LLVMModuleRef, LLVMBuilderRef);
extern void allvm_irstate_free(IRState*);
extern void allvm_irstate_module_done(IRState*);

#endif
