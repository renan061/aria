#include <stdio.h> // TODO: Remove
#include <stdlib.h> // TODO: Remove
#include <strings.h>

#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>

#include "ast.h"
#include "parser.h"
#include "scanner.h"
#include "sem.h"
#include "backend.h"

#define TAG_HELP    'h'
#define TAG_BUILD   'b'
#define TAG_RUN     'r'

#define TAG_BUILD_TEXT  "builds the program to LLVM IR"
#define TAG_RUN_TEXT    "runs the program using LLVM's interpreter"

static int execute_module(LLVMModuleRef module);
static void checkargs(int argc, char* argv[]);
static void checkhelp(int argc, char* argv[]);

int main(int argc, char* argv[]) {
    checkhelp(argc, argv);
    checkargs(argc, argv);

    bool build = false, run = false;

    // TODO: This is badly done
    char* flag = argv[1];
    if (flag[0] != '-') {
        printf("error: invalid flag (see 'aria -h')\n");
        return 1;
    }
    switch (flag[1]) {
    case TAG_BUILD:
        build = true;
        break;
    case TAG_RUN:
        run = build = true;
        break;
    default:
        printf("error: invalid flag (see 'aria -h')\n");
        return 1;   
    }

    scanner_setup(argv[2]);
    yyparse();
    scanner_clean();
    sem_analyse(ast);
    LLVMModuleRef module = compile(ast);

    if (build) {
        if (LLVMWriteBitcodeToFile(module, "aria.bc") != 0) {
            printf("error: couldn't write bitcode to file\n");
            exit(1);
        }
    }
    
    if (run) {
        return execute_module(module);
    }

    return 0;
}

// ==================================================
//
//  Auxiliary
//
// ==================================================

static void checkhelp(int argc, char* argv[]) {
    if (argc != 2 || (argv[1][0] != '-' && argv[1][1] != TAG_HELP)) {
        return;
    }

    printf("Usage:\n\n");
    printf("\taria [command-line-options] [input-file]\n\n");

    printf("Options:\n\n");
    printf("\t-%c\t\t" TAG_BUILD_TEXT   "\n", TAG_BUILD);
    printf("\t-%c\t\t" TAG_RUN_TEXT     "\n", TAG_RUN);

    printf("\n");
    exit(0);
}

static void checkargs(int argc, char* argv[]) {
    if (argc < 2) {
        printf("error: missing arguments to main (see 'aria -h')\n");
        exit(1);
    }
}

static int execute_module(LLVMModuleRef module) {
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

    LLVMGenericValueRef res = LLVMRunFunction(engine, main_function, 0, NULL);
    LLVMDisposeExecutionEngine(engine);
    return (int) LLVMGenericValueToInt(res, 0);
}
