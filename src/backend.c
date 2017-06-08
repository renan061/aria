#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove
#include <strings.h> // TODO: Remove

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"
#include "parser.h" // for the tokens

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL)

// TODO: Remove
#define TODO assert(NULL);

// TODO
#define TEMP "_t"
#define STR "_str"

#define LABEL "label"
#define LABEL_IF_TRUE		LABEL "_if_true"
#define LABEL_IF_END		LABEL "_if_end"
#define LABEL_IF_ELSE_TRUE	LABEL "_if_else_true"
#define LABEL_IF_ELSE_FALSE	LABEL "_if_else_false"
#define LABEL_IF_ELSE_END	LABEL "_if_else_end"
#define LABEL_WHILE			LABEL "_while"
#define LABEL_WHILE_LOOP	LABEL "_while_loop"
#define LABEL_WHILE_END		LABEL "_while_end"

// TODO: LLVM Types / Values
#define LLVM_VOID_TYPE LLVMVoidType()
#define LLVM_BOOLEAN_TYPE LLVMIntType(1)
#define LLVM_INTEGER_TYPE LLVMInt32Type()
#define LLVM_FLOAT_TYPE LLVMDoubleType()
#define LLVM_STRING_TYPE LLVMPointerType(LLVMInt8Type(), 0);

#define LLVM_TRUE_VALUE LLVMConstInt(LLVM_BOOLEAN_TYPE, true, false)
#define LLVM_FALSE_VALUE LLVMConstInt(LLVM_BOOLEAN_TYPE, false, false)

#define LLVM_APPEND_BLOCK(state, name) \
	LLVMPositionBuilderAtEnd( \
		state->builder, \
		LLVMAppendBasicBlock(state->function, name) \
	) \

// Primitive types
static Type* boolean_;
static Type* integer_;
static Type* float_;
static Type* string_;

typedef struct IRState {
	LLVMModuleRef module;
	LLVMBuilderRef builder;

	// TODO
	LLVMValueRef printf;

	// Current function
    LLVMValueRef function;
} IRState;

// LLVM (TODO: New Module)
static void llvm_function_declaration(LLVMModuleRef, Declaration*);
static LLVMTypeRef llvm_type(Type* type);
static void llvm_return(LLVMBuilderRef, LLVMValueRef);

// Auxiliary
static LLVMValueRef typezerovalue(Type*);

// TODO
static void backend_body(IRState*, Body*);
static void backend_declaration(IRState*, Declaration*);
static void backend_definition(IRState*, Definition*);
static void backend_block(IRState*, Block*);
static void backend_statement(IRState*, Statement*);
static void backend_variable(IRState*, Variable*);
static void backend_expression(IRState*, Expression*);
static void backend_condition(IRState*, Expression*,
	LLVMBasicBlockRef, LLVMBasicBlockRef);
static LLVMValueRef backend_function_call(IRState*, FunctionCall*);

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
	// TODO: Should declare something like `extern printf` in the code...
	LLVMTypeRef printf_param_types[] = {LLVMPointerType(LLVMInt8Type(), 0)};
	LLVMTypeRef printf_type = LLVMFunctionType(LLVMInt32Type(),
		printf_param_types, 1, true); 
	state.printf = LLVMAddFunction(state.module, "printf", printf_type);

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
			backend_declaration(state, b->declaration);
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
		declaration->variable->llvm_value = LLVMBuildAlloca(
			state->builder,
			llvm_type(declaration->variable->type),
			declaration->variable->id->name
		);
		break;
	case DECLARATION_FUNCTION:
		llvm_function_declaration(state->module, declaration);
		state->function = declaration->llvm_value;
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
			variable->llvm_value = LLVMAddGlobal(
				state->module,
				llvm_type(variable->type),
				variable->id->name
			);
			backend_expression(state, expression);
			LLVMSetInitializer(variable->llvm_value, expression->llvm_value);
		} else {
			backend_declaration(state, definition->variable.declaration);
			backend_expression(state, expression);
			LLVMBuildStore(
				state->builder,
				expression->llvm_value,
				variable->llvm_value
			);
		}
		break;
	}
	case DEFINITION_CONSTRUCTOR:
		TODO;
		break;
	case DEFINITION_FUNCTION:
		// `backed_declaration` sets state->function internally
		backend_declaration(state, definition->function.declaration);
		LLVM_APPEND_BLOCK(state, LABEL "_function_entry");
		backend_block(state, definition->function.block);
		// returns with the zero value of the function's return type
		LLVM_APPEND_BLOCK(state, LABEL "_invariant_return");
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
			backend_block(state, block->next);
		}
		return;
	}

	for (Block* b = block; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			backend_declaration(state, b->declaration);
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
			statement->assignment.expression->llvm_value,
			statement->assignment.variable->llvm_value
		);
		break;
	case STATEMENT_FUNCTION_CALL:
		backend_function_call(state, statement->function_call);
		break;
	case STATEMENT_WHILE_WAIT:
		// break;
		TODO;
	case STATEMENT_SIGNAL:
		// break;
		TODO;
	case STATEMENT_BROADCAST:
		// break;
		TODO;
	case STATEMENT_RETURN:
		if (statement->return_) {
			backend_expression(state, statement->return_);
			llvm_return(state->builder, statement->return_->llvm_value);
		} else {
			llvm_return(state->builder, NULL);
		}
		break;
	case STATEMENT_IF: {
		LLVMBasicBlockRef
			bt = LLVMAppendBasicBlock(state->function, LABEL_IF_TRUE),
			be = LLVMAppendBasicBlock(state->function, LABEL_IF_END);
		backend_condition(state, statement->if_.expression, bt, be);
		// If
		LLVMPositionBuilderAtEnd(state->builder, bt);
		backend_block(state, statement->if_.block);
		LLVMBuildBr(state->builder, be);
		// End
		LLVMPositionBuilderAtEnd(state->builder, be);
		break;
	}
	case STATEMENT_IF_ELSE: {
		LLVMBasicBlockRef
			bt = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_TRUE),
			bf = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_FALSE),
			be = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_END);
		backend_condition(state, statement->if_else.expression, bt, bf);
		// If
		LLVMPositionBuilderAtEnd(state->builder, bt);
		backend_block(state, statement->if_else.if_block);
		LLVMBuildBr(state->builder, be);
		// Else
		LLVMPositionBuilderAtEnd(state->builder, bf);
		backend_block(state, statement->if_else.else_block);
		LLVMBuildBr(state->builder, be);
		// End
		LLVMPositionBuilderAtEnd(state->builder, be);
		break;
	}
	case STATEMENT_WHILE: {
		LLVMBasicBlockRef
			bw = LLVMAppendBasicBlock(state->function, LABEL_WHILE),
			bl = LLVMAppendBasicBlock(state->function, LABEL_WHILE_LOOP),
			be = LLVMAppendBasicBlock(state->function, LABEL_WHILE_END);
		LLVMBuildBr(state->builder, bw);
		// While
		LLVMPositionBuilderAtEnd(state->builder, bw);
		backend_condition(state, statement->while_.expression, bl, be);
		// Loop
		LLVMPositionBuilderAtEnd(state->builder, bl);
		backend_block(state, statement->while_.block);
		LLVMBuildBr(state->builder, bw);
		// End
		LLVMPositionBuilderAtEnd(state->builder, be);
		break;
	}
	case STATEMENT_SPAWN:
		// break;
		TODO;
	case STATEMENT_BLOCK:
		backend_block(state, statement->block);
		break;
	default:
		UNREACHABLE;
	}
}

static void backend_variable(IRState* state, Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID:
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
		expression->llvm_value = LLVMConstInt(llvm_type(expression->type),
			expression->literal_boolean, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->llvm_value = LLVMConstInt(llvm_type(expression->type),
			expression->literal_integer, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->llvm_value = LLVMConstReal(llvm_type(expression->type),
			expression->literal_float);
		break;
	case EXPRESSION_LITERAL_STRING: {
		// TODO: LLVMGlobalString ???

		// Global
		const char* string = expression->literal_string;
		size_t len = strlen(string);
		LLVMValueRef llvm_global = LLVMAddGlobal(state->module,
			LLVMArrayType(LLVMInt8Type(), len + 1), STR);
		LLVMSetInitializer(llvm_global, LLVMConstString(string, len, false));
		LLVMSetVisibility(llvm_global, LLVMHiddenVisibility);

		// Expression
		expression->llvm_value = LLVMBuildPointerCast(
			state->builder,
			llvm_global,
			llvm_type(expression->type),
			TEMP
		);
		break;
	}
	case EXPRESSION_VARIABLE:
		backend_variable(state, expression->variable);
		expression->llvm_value = LLVMBuildLoad(
			state->builder,
			expression->variable->llvm_value,
			TEMP
		);
		break;
	case EXPRESSION_FUNCTION_CALL:
		expression->llvm_value =
			backend_function_call(state, expression->function_call);
		break;
	case EXPRESSION_UNARY:
		switch (expression->unary.token) {
		case '-':
			backend_expression(state, expression->unary.expression);
			if (expression->type == integer_) {
				expression->llvm_value = LLVMBuildNeg(state->builder,
					expression->unary.expression->llvm_value, TEMP);
			} else if (expression->type == float_) {
				expression->llvm_value = LLVMBuildFNeg(state->builder,
					expression->unary.expression->llvm_value, TEMP);
			} else {
				UNREACHABLE;
			}
			break;
		case TK_NOT:
			goto CONDITION_EXPRESSION;
		default:
			UNREACHABLE;
		}
		break;
	case EXPRESSION_BINARY:
		switch (expression->binary.token) {
		case TK_OR:
		case TK_AND:
		case TK_EQUAL:
		case TK_LEQUAL:
		case TK_GEQUAL:
		case '<':
		case '>':
			goto CONDITION_EXPRESSION;

		// Macro to be used by the [+, -, *, /] operations
		#define BINARY_ARITHMETICS(e, s, ifunc, ffunc) \
			if (e->type == integer_) { \
				e->llvm_value = ifunc(s->builder, \
					e->binary.left_expression->llvm_value, \
					e->binary.right_expression->llvm_value, TEMP); \
			} else if (e->type == float_) { \
				e->llvm_value = ffunc(s->builder, \
					e->binary.left_expression->llvm_value, \
					e->binary.right_expression->llvm_value, TEMP); \
			} else { \
				UNREACHABLE; \
			} \
		// End macro

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
		default:
			UNREACHABLE;
		}
		break;
	case EXPRESSION_CAST:
		backend_expression(state, expression->cast);
		if (expression->cast->type == integer_ &&
			expression->type == float_) {
			// Integer to Float
			expression->llvm_value = LLVMBuildSIToFP(
				state->builder,
				expression->cast->llvm_value,
				llvm_type(expression->type),
				TEMP
			);
		} else if (expression->cast->type == float_ &&
			expression->type == integer_) {
			// Float to Integer
			expression->llvm_value = LLVMBuildFPToSI(
				state->builder,
				expression->cast->llvm_value,
				llvm_type(expression->type),
				TEMP
			);
		} else {
			UNREACHABLE;
		}
		break;
	default:
		UNREACHABLE;
	}

	return;

	CONDITION_EXPRESSION: { // expression ? true : false
		LLVMBasicBlockRef
			block_true	= LLVMAppendBasicBlock(state->function, LABEL "_a"),
			block_false	= LLVMAppendBasicBlock(state->function, LABEL "_b"),
			block_phi	= LLVMAppendBasicBlock(state->function, LABEL "_phi");

		backend_condition(state, expression, block_true, block_false);
		LLVMPositionBuilderAtEnd(state->builder, block_true);
		LLVMBuildBr(state->builder, block_phi);
		LLVMPositionBuilderAtEnd(state->builder, block_false);
		LLVMBuildBr(state->builder, block_phi);
		LLVMPositionBuilderAtEnd(state->builder, block_phi);

		LLVMValueRef
			phi = LLVMBuildPhi(state->builder, LLVM_BOOLEAN_TYPE, TEMP "_phi"),
			incoming_values[2] = {LLVM_TRUE_VALUE, LLVM_FALSE_VALUE};
		LLVMBasicBlockRef incoming_blocks[2] = {block_true, block_false};
		LLVMAddIncoming(phi, incoming_values, incoming_blocks, 2);

		expression->llvm_value = phi;
	}
}

static void backend_condition(IRState* state, Expression* expression,
	LLVMBasicBlockRef lt, LLVMBasicBlockRef lf) {

	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
	case EXPRESSION_LITERAL_INTEGER:
	case EXPRESSION_LITERAL_FLOAT:
	case EXPRESSION_LITERAL_STRING:
	case EXPRESSION_VARIABLE:
	case EXPRESSION_FUNCTION_CALL:
		goto EXPRESSION_CONDITION;
	case EXPRESSION_UNARY:
		switch (expression->unary.token) {
		case TK_NOT:
			backend_condition(state, expression->unary.expression, lf, lt);
			break;
		case '-':
			goto EXPRESSION_CONDITION;
		default:
			UNREACHABLE;
		}
		break;
	case EXPRESSION_BINARY:
		switch (expression->binary.token) {		
		case TK_OR: {
			LLVMBasicBlockRef label =
				LLVMAppendBasicBlock(state->function, LABEL "_or");
			backend_condition(state, expression->binary.left_expression,
				lt, label);
			LLVMPositionBuilderAtEnd(state->builder, label);
			backend_condition(state, expression->binary.right_expression,
				lt, lf);
			break;
		}
		case TK_AND: {
			LLVMBasicBlockRef label =
				LLVMAppendBasicBlock(state->function, LABEL "_and");
			backend_condition(state, expression->binary.left_expression,
				label, lf);
			LLVMPositionBuilderAtEnd(state->builder, label);
			backend_condition(state, expression->binary.right_expression,
				lt, lf);
			break;
		}
		case TK_EQUAL: {
			Expression* l = expression->binary.left_expression;
			Expression* r = expression->binary.right_expression;
			backend_expression(state, l);
			backend_expression(state, r);

			// TODO: More readable
			if (l->type == boolean_ && r->type == boolean_) {
				expression->llvm_value = LLVMBuildICmp(state->builder,
					LLVMIntEQ, l->llvm_value, r->llvm_value, TEMP);
			} else if (l->type == integer_ && r->type == integer_) {
				expression->llvm_value = LLVMBuildICmp(state->builder,
					LLVMIntEQ, l->llvm_value, r->llvm_value, TEMP);
			} else if (l->type == float_ && r->type == float_) {
				expression->llvm_value = LLVMBuildFCmp(state->builder,
					LLVMRealOEQ, l->llvm_value, r->llvm_value, TEMP);
			} else {
				UNREACHABLE;
			}
			LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
			break;
		}

		// Macro to be used by the [<=, >=, <, >] operations
		#define BINARY_COMPARISSON(lo, ro) { \
			Expression* l = expression->binary.left_expression; \
			Expression* r = expression->binary.right_expression; \
			backend_expression(state, l); \
			backend_expression(state, r); \
			expression->llvm_value = \
				(l->type == integer_ && r->type == integer_) ? \
					LLVMBuildICmp(state->builder, lo, l->llvm_value, \
						r->llvm_value, TEMP) \
				: (l->type == float_ && r->type == float_) ? \
					LLVMBuildFCmp(state->builder, ro, l->llvm_value, \
						r->llvm_value, TEMP) \
				: (UNREACHABLE, NULL); \
			LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf); \
		} \
		// End macro

		case TK_LEQUAL:
			BINARY_COMPARISSON(LLVMIntSLE, LLVMRealOLE);
			break;
		case TK_GEQUAL:
			BINARY_COMPARISSON(LLVMIntSGE, LLVMRealOGE);
			break;
		case '<':
			BINARY_COMPARISSON(LLVMIntSLT, LLVMRealOLT);
			break;
		case '>':
			BINARY_COMPARISSON(LLVMIntSGT, LLVMRealOGT);
			break;
		case '+':
		case '-':
		case '*':
		case '/':
			goto EXPRESSION_CONDITION;
		default:
			UNREACHABLE;
		}
		break;
	case EXPRESSION_CAST:
		goto EXPRESSION_CONDITION;
	default:
		UNREACHABLE;
	}

	return;

	EXPRESSION_CONDITION: {
		backend_expression(state, expression);
		LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
	}
}

static LLVMValueRef backend_function_call(IRState* state, FunctionCall* call) {
	Expression* arg;
	int n = 0;

	for (arg = call->arguments; arg; arg = arg->next, n++) {
		backend_expression(state, arg);
	}

	LLVMValueRef arguments[n];

	arg = call->arguments;
	for (int i = 0; i < n; arg = arg->next, i++) {
		arguments[i] = arg->llvm_value;
	}

	// TODO: Remove this gambiarra
	if (!strcmp(call->basic->name, "print")) {
		return LLVMBuildCall(state->builder, state->printf, arguments, n, "");
	}

	return LLVMBuildCall(state->builder, call->declaration->llvm_value,
		arguments, n, "");
}

// ==================================================
//
//	LLVM
//
// ==================================================

// sets the value reference for the function prototype inside declaration
static void llvm_function_declaration(LLVMModuleRef module,
	Declaration* declaration) {

	assert(declaration->tag == DECLARATION_FUNCTION);
	assert(declaration->function.id);
	assert(declaration->function.type);
	
	// Counting the number of parameters
	Declaration* parameter = declaration->function.parameters;
	unsigned int paramCount = 0;
	for (Declaration* p = parameter; p; p = p->next, paramCount++);

	// Creating a list of parameters
	LLVMTypeRef paramTypes[paramCount]; // TODO: Won't this be destroyed?
	parameter = declaration->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		paramTypes[i] = llvm_type(parameter->variable->type);
	}

	// Creating the function prototype and setting it as the current function
	declaration->llvm_value = LLVMAddFunction(
		/* Module		*/ module,
		/* FunctionName	*/ declaration->function.id->name,
		/* FunctionRef	*/ LLVMFunctionType(
			/* ReturnType	*/ llvm_type(declaration->function.type),
			/* ParamTypes	*/ paramTypes,
			/* ParamCount	*/ paramCount,
			/* IsVarArg		*/ false
		)
	);

	// Setting the names for the parameters
	parameter = declaration->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		parameter->variable->llvm_value = LLVMGetParam(
			declaration->llvm_value,
			i
		);
		LLVMSetValueName(
			parameter->variable->llvm_value,
			parameter->variable->id->name
		);
	}
}

static LLVMTypeRef llvm_type(Type* type) {
	switch (type->tag) {
	case TYPE_VOID:
		return LLVM_VOID_TYPE;
	case TYPE_ID:
		if (type == boolean_) {
			return LLVM_BOOLEAN_TYPE;
		}
		if (type == integer_) {
			return LLVM_INTEGER_TYPE;
		}
		if (type == float_) {
			return LLVM_FLOAT_TYPE;
		}
		if (type == string_) {
			return LLVM_STRING_TYPE;
		}
		UNREACHABLE;
	case TYPE_ARRAY:
		TODO;
	default:
		UNREACHABLE;
	}
}

static void llvm_return(LLVMBuilderRef builder, LLVMValueRef llvm_value) {
	if (llvm_value) {
		LLVMBuildRet(builder, llvm_value);
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
		TODO; // string zero value (nil pointer ? ""?)
		break;
	default:
		UNREACHABLE;
	}
}
