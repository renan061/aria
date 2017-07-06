#include <stdio.h> // TODO: Remove
#include <stdlib.h> // TODO: Remove

#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>

#include "ast.h"
#include "parser.h"
#include "scanner.h"
#include "sem.h"
#include "backend.h"

static int executeModule(LLVMModuleRef module);

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("error: missing arguments to main\n");
    	return 1;
    }

	scanner_setup(argv[1]);
	yyparse();
	scanner_clean();
	sem_analyse(ast);
	LLVMModuleRef module = backend_compile(ast);

	// Bytecode
	if (LLVMWriteBitcodeToFile(module, "aria.bc") != 0) {
        printf("Error writing bitcode to file\n");
        exit(1);
    }

	return executeModule(module);
}

static int executeModule(LLVMModuleRef module) {
	// TODO: "LLVM ERROR: Target does not support MC emission!"
	LLVMInitializeNativeAsmPrinter();
	// LLVMInitializeNativeAsmParser();

	LLVMExecutionEngineRef engine;
	char* error = NULL;
	LLVMLinkInMCJIT();
	LLVMInitializeNativeTarget();
	if (LLVMCreateExecutionEngineForModule(&engine, module, &error) != 0) {
		printf("error: failed to create execution engine\n");
		exit(1);
	}
	if (error) {
		LLVMDisposeMessage(error);
		printf("error: %s", error);
		exit(1);
	}
	LLVMValueRef main_function = LLVMGetNamedFunction(module, "main");
	if (!main_function) {
		printf("error: main function not found\n");
		exit(1);
	}

	LLVMGenericValueRef result = LLVMRunFunction(engine, main_function, 0, NULL);
	LLVMDisposeExecutionEngine(engine);
	return (int) LLVMGenericValueToInt(result, 0);
}
