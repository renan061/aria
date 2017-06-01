#include <stdbool.h>
#include <stdio.h> // TODO: Remove

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>

#include "ast.h"

// Sum

// define i32 @sum(i32, i32) #0 {
// 	%3 = alloca i32, align 4
// 	%4 = alloca i32, align 4
// 	store i32 %0, i32* %3, align 4
// 	store i32 %1, i32* %4, align 4
// 	%5 = load i32, i32* %3, align 4
// 	%6 = load i32, i32* %4, align 4
// 	%7 = add nsw i32 %5, %6
// 	ret i32 %7
// }

// static void sum() {
// 	// Creating the prototype
// 	LLVMTypeRef params[] = {LLVMInt32Type(), LLVMInt32Type()};
// 	LLVMTypeRef function = LLVMFunctionType(LLVMInt32Type(), params, 2, false);
// 	LLVMValueRef prototype = LLVMAddFunction(module, "sum", function);

// 	// Basic blocks
// 	LLVMBasicBlockRef entry = LLVMAppendBasicBlock(prototype, "entry");

// 	LLVMBuilderRef builder = LLVMCreateBuilder();
// 	LLVMPositionBuilderAtEnd(builder, entry);

// 	LLVMValueRef temp = LLVMBuildAdd(builder,
// 		LLVMGetParam(prototype, 0),
// 		LLVMGetParam(prototype, 1),
// 		"tmp"
// 	);
// 	LLVMBuildRet(builder, temp);
// }

LLVMModuleRef backend_compile(Program* program) {
	LLVMModuleRef module = LLVMModuleCreateWithName("aria-llvm");

	// Creating the prototype
	LLVMTypeRef function = LLVMFunctionType(LLVMInt32Type(), NULL, 0, false);
	LLVMValueRef prototype = LLVMAddFunction(module, "main", function);

	// Basic blocks
	LLVMBasicBlockRef entry = LLVMAppendBasicBlock(prototype, "entry");

	LLVMBuilderRef builder = LLVMCreateBuilder();
	LLVMPositionBuilderAtEnd(builder, entry);

	LLVMValueRef temp = LLVMConstInt(LLVMInt32Type(), 0, false);
	LLVMBuildRet(builder, temp);

	// Analysis
	char* error = NULL;
	LLVMVerifyModule(module, LLVMAbortProcessAction, &error);
	LLVMDisposeMessage(error);

	return module;
}