#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "alloc.h"
#include "allvm.h"

IRState* allvm_irstate_new(LLVMModuleRef module, LLVMBuilderRef builder) {
	IRState* s;
	MALLOC(s, IRState);
	s->module = module;
	s->builder = builder;
	s->function = NULL;
	s->block = NULL;
	s->self = NULL;
	s->main = false;
	s->initializer = false;
	return s;
}

void allvm_irstate_free(IRState* state) {
	free(state);
}

void allvm_irstate_module_done(IRState* state) {
	char* error = NULL;
	LLVMVerifyModule(state->module, LLVMAbortProcessAction, &error);
	if (error) {
		LLVMDisposeMessage(error);
	}

	LLVMDisposeBuilder(state->builder);
}
