#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL);

// TODO: Remove
#define TODO assert(NULL);

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

// Primitive types
static Type* boolean_;
static Type* integer_;
static Type* float_;
static Type* string_;

typedef struct IRState {
	LLVMModuleRef module;
	LLVMBuilderRef builder;

	// Current function
    LLVMValueRef function;
} IRState;

// LLVM (TODO: New Module)
static LLVMTypeRef llvm_printf(void);
static LLVMValueRef llvm_function_declaration(LLVMModuleRef, Declaration*);
static LLVMTypeRef llvm_type(Type* type);
static void llvm_return(LLVMBuilderRef, LLVMValueRef);

// Auxiliary
static LLVMValueRef typezerovalue(Type*);

// TODO
static void backend_body(IRState*, Body*);
static void backend_definition(IRState*, Definition*);
static void backend_block(IRState*, Block*);
static void backend_statement(IRState*, Statement*);
static void backend_expression(IRState*, Expression*);

// ==================================================
//
//	Implementation
//
// ==================================================

LLVMModuleRef backend_compile(Program* program) {
	// Setup
	boolean_ = ast_type_boolean();
	integer_ = ast_type_integer();
	float_ = ast_type_float();
	string_ = ast_type_string();

	// LLVM Setup
	IRState state = {
		/* module	*/ LLVMModuleCreateWithName("main.aria"),
		/* builder	*/ LLVMCreateBuilder(),
		/* function	*/ NULL
	};

	// Includes
	LLVMAddFunction(state.module, "printf", llvm_printf());

	backend_body(&state, program->body);

	// Analysis
	char* error = NULL;
	LLVMVerifyModule(state.module, LLVMAbortProcessAction, &error);
	LLVMDisposeMessage(error);

	// LLVM Teardown
	LLVMDisposeBuilder(state.builder);

	return state.module;
}

static void backend_body(IRState* state, Body* body) {
	if (body->tag == BODY) {
		if (body->next) {
			backend_body(state, body->next);
		}
		return;
	}

	for (Body* b = body; b; b = b->next) {
		switch (b->tag) {
		case BODY_DECLARATION:
			TODO;
			break;
		case BODY_DEFINITION:
			backend_definition(state, b->definition);
			break;
		default:
			UNREACHABLE;
		}
	}
}

static void backend_definition(IRState* state, Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE:
		TODO;
		break;
	case DEFINITION_CONSTRUCTOR:
		TODO;
		break;
	case DEFINITION_FUNCTION:
		// TODO: Move to backend_declaration
		{
			Declaration* d = definition->function.declaration;
			state->function = llvm_function_declaration(state->module, d);
		}

		// backend_declaration(definition->function.declaration);
		backend_block(state, definition->function.block);
		LLVMPositionBuilderAtEnd(
			/* Builder	*/ state->builder,
			/* Block	*/ LLVMAppendBasicBlock(state->function, "end")
		);
		llvm_return(state->builder,
			typezerovalue(definition->function.declaration->function.type));
		break;
	case DEFINITION_METHOD:
		TODO;
		break;
	case DEFINITION_TYPE:
		TODO;
		break;
	default:
		UNREACHABLE;
	}
}

static void backend_block(IRState* state, Block* block) {
	if (block->tag == BLOCK) {
		if (block->next) {
			LLVMPositionBuilderAtEnd(
				/* Builder	*/ state->builder,
				/* Block	*/ LLVMAppendBasicBlock(state->function, "entry")
			);
			backend_block(state, block->next);
		}
		return;
	}

	for (Block* b = block; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			TODO;
			break;
		case BLOCK_DEFINITION:
			backend_definition(state, b->definition);
			break;
		case BLOCK_STATEMENT:
			backend_statement(state, b->statement);
			break;
		default:
			UNREACHABLE;
		}
	}
}

static void backend_statement(IRState* state, Statement* statement) {
	switch (statement->tag) {
	// case STATEMENT_ASSIGNMENT:
	// case STATEMENT_FUNCTION_CALL:
	// case STATEMENT_WHILE_WAIT:
	// case STATEMENT_SIGNAL:
	// case STATEMENT_BROADCAST:
	case STATEMENT_RETURN:
		if (statement->return_) {
			backend_expression(state, statement->return_);
			llvm_return(state->builder, statement->return_->temp);
		} else {
			llvm_return(state->builder, NULL);
		}
		break;
	// case STATEMENT_IF:
	// case STATEMENT_IF_ELSE:
	// case STATEMENT_WHILE:
	// case STATEMENT_SPAWN:
	// case STATEMENT_BLOCK:
	default:
		UNREACHABLE;
	}
}

static void backend_expression(IRState* state, Expression* expression) {
	switch (expression->tag) {
	// case EXPRESSION_LITERAL_BOOLEAN:
	case EXPRESSION_LITERAL_INTEGER:
		expression->temp = LLVMConstInt(
			/* Type			*/ llvm_type(expression->type),
			/* Value		*/ expression->literal_integer,
			/* SignExtend	*/ false // TODO: Deveria?
		);
		break;
	// case EXPRESSION_LITERAL_FLOAT:
	// case EXPRESSION_LITERAL_STRING:
	// case EXPRESSION_VARIABLE:
	// case EXPRESSION_FUNCTION_CALL:
	// case EXPRESSION_UNARY:
	// case EXPRESSION_BINARY:
	// case EXPRESSION_CAST:
	default:
		UNREACHABLE;
	}
}

// ==================================================
//
//	LLVM
//
// ==================================================

static LLVMTypeRef llvm_printf(void) {
	LLVMTypeRef paramTypes[] = {LLVMPointerType(LLVMInt8Type(), 0)};
	return LLVMFunctionType(LLVMInt32Type(), paramTypes, 1, true); 
}

// returns the value reference for the function prototype
static LLVMValueRef llvm_function_declaration(LLVMModuleRef module,
	Declaration* prototype) {

	assert(prototype->tag == DECLARATION_FUNCTION);
	assert(prototype->function.id);
	assert(prototype->function.type);
	
	// Counting the number of parameters
	Declaration* parameter = prototype->function.parameters;
	unsigned int paramCount = 0;
	for (Declaration* p = parameter; p; p = p->next, paramCount++);

	// Creating a list of parameters
	LLVMTypeRef paramTypes[paramCount]; // TODO: Won't this be destroyed?
	parameter = prototype->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		paramTypes[i] = llvm_type(parameter->variable->type);
	}

	// Creating the function prototype and setting it as the current function
	LLVMValueRef function = LLVMAddFunction(
		/* Module		*/ module,
		/* FunctionName	*/ prototype->function.id->name,
		/* FunctionRef	*/ LLVMFunctionType(
			/* ReturnType	*/ llvm_type(prototype->function.type),
			/* ParamTypes	*/ paramTypes,
			/* ParamCount	*/ paramCount,
			/* IsVarArg		*/ false
		)
	);

	// Setting the names for the parameters
	parameter = prototype->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		LLVMSetValueName(
			/* Value	*/ LLVMGetParam(function, i),
			/* Name		*/ parameter->variable->id->name
		);
	}

	return function;
}

static LLVMTypeRef llvm_type(Type* type) {
	switch (type->tag) {
		case TYPE_VOID:
			return LLVMVoidType();
		case TYPE_ID:
			if (type == boolean_) {
				return LLVMIntType(1);
			}
			if (type == integer_) {
				return LLVMInt32Type();
			}
			TODO;
			break; // TODO
		default:
			UNREACHABLE;
	}
}

static void llvm_return(LLVMBuilderRef builder, LLVMValueRef temp) {
	if (temp) {
		LLVMBuildRet(builder, temp);
	} else {
		LLVMBuildRetVoid(builder);
	}
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO: Find a better way to write this...
static LLVMValueRef typezerovalue(Type* type) {
	switch (type->tag) {
		case TYPE_VOID:
			return NULL;
		case TYPE_ID:
			if (type == boolean_) {
				return LLVMConstInt(llvm_type(type), false, false);
			}
			if (type == integer_) {
				return LLVMConstInt(llvm_type(type), 0, false);
			}
			if (type == float_) {
				return LLVMConstReal(llvm_type(type), 0.0);
			}
			TODO;
			break; // TODO
		default:
			UNREACHABLE;
	}
}
