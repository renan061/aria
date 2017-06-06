#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"
#include "parser.h" // for the tokens

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL);

// TODO: Remove
#define TODO assert(NULL);

#define TEMP "_t"

#define LLVM_APPEND_BLOCK(state, name) \
	LLVMPositionBuilderAtEnd( \
		state->builder, \
		LLVMAppendBasicBlock(state->function, name) \
	); \

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
static void backend_variable(IRState*, Variable*);
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

// TODO: Maybe remove?
static void backend_declaration(IRState* state, Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		TODO;
	case DECLARATION_FUNCTION:
		state->function = llvm_function_declaration(state->module, declaration);
		break;
	default:
		UNREACHABLE;
	}
}

static void backend_definition(IRState* state, Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Variable* variable = definition->variable.declaration->variable;
		Expression* expression = definition->variable.expression;

		if (variable->global) {
			TODO;
		} else {
			variable->temp = LLVMBuildAlloca(
				state->builder,
				llvm_type(variable->type),
				variable->id->name
			);
			backend_expression(state, expression);
			LLVMBuildStore(state->builder, expression->temp, variable->temp);
		}
		break;
	}
	case DEFINITION_CONSTRUCTOR:
		TODO;
		break;
	case DEFINITION_FUNCTION:
		// `backed_declaration` sets state->function internally
		backend_declaration(state, definition->function.declaration);
		backend_block(state, definition->function.block);
		// returns with the zero value of the function's return type
		LLVM_APPEND_BLOCK(state, "invariant-return");
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
			LLVM_APPEND_BLOCK(state, "entry");
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
	case STATEMENT_ASSIGNMENT:
		backend_variable(state, statement->assignment.variable);
		backend_expression(state, statement->assignment.expression);
		LLVMBuildStore(
			state->builder,
			statement->assignment.expression->temp,
			statement->assignment.variable->temp
		);
		break;
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

static void backend_variable(IRState* state, Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID:
		if (variable->global) {
			TODO;
		}
		break;
	case VARIABLE_INDEXED:
		TODO;
	default:
		UNREACHABLE;
	}
}

static void backend_expression(IRState* state, Expression* expression) {
	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		expression->temp = LLVMConstInt(llvm_type(expression->type),
			expression->literal_boolean, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->temp = LLVMConstInt(llvm_type(expression->type),
			expression->literal_integer, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->temp = LLVMConstReal(llvm_type(expression->type),
			expression->literal_float);
		break;
	// case EXPRESSION_LITERAL_STRING:
	case EXPRESSION_VARIABLE:
		expression->temp = LLVMBuildLoad(state->builder,
			expression->variable->temp, TEMP);
		break;
	// case EXPRESSION_FUNCTION_CALL:
	case EXPRESSION_UNARY:
		switch (expression->unary.token) {
		case '-':
			backend_expression(state, expression->unary.expression);
			if (expression->type == integer_) {
				expression->temp = LLVMBuildNeg(state->builder,
					expression->unary.expression->temp, TEMP);
			} else if (expression->type == float_) {
				expression->temp = LLVMBuildFNeg(state->builder,
					expression->unary.expression->temp, TEMP);
			} else {
				UNREACHABLE;
			}
			break;
		case TK_NOT:
			TODO;
			break;
		}
		break;
	case EXPRESSION_BINARY:
		// Macro to be used by the [+, -, *, /] operations
		#define BINARY_ARITHMETICS(e, s, ifunc, ffunc) \
			if (e->type == integer_) { \
				e->temp = ifunc(s->builder, e->binary.left_expression->temp, \
					e->binary.right_expression->temp, TEMP); \
			} else if (e->type == float_) { \
				e->temp = ffunc(s->builder, e->binary.left_expression->temp, \
					e->binary.right_expression->temp, TEMP); \
			} else { \
				UNREACHABLE; \
			} \
		// End macro

		switch (expression->binary.token) {
		case TK_OR:
		case TK_AND:
		case TK_EQUAL:
		case TK_LEQUAL:
		case TK_GEQUAL:
		case '<':
		case '>':
			TODO; // goto DEFAULT_EXPRESSION;
		case '+':
			backend_expression(state, expression->binary.left_expression);
			backend_expression(state, expression->binary.right_expression);
			BINARY_ARITHMETICS(expression, state, LLVMBuildAdd, LLVMBuildFAdd);
			break;
		case '-':
			backend_expression(state, expression->binary.left_expression);
			backend_expression(state, expression->binary.right_expression);
			BINARY_ARITHMETICS(expression, state, LLVMBuildSub, LLVMBuildFSub);
			break;
		case '*':
			backend_expression(state, expression->binary.left_expression);
			backend_expression(state, expression->binary.right_expression);
			BINARY_ARITHMETICS(expression, state, LLVMBuildMul, LLVMBuildFMul);
			break;
		case '/':
			backend_expression(state, expression->binary.left_expression);
			backend_expression(state, expression->binary.right_expression);
			BINARY_ARITHMETICS(expression, state, LLVMBuildSDiv, LLVMBuildFDiv);
			break;
		}
		break;
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
		parameter->variable->temp = LLVMGetParam(function, i);
		LLVMSetValueName(
			/* Value	*/ parameter->variable->temp,
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
			if (type == float_) {
				return LLVMDoubleType();
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
