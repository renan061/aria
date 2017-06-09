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
#define TODO assert(NULL)

// TODO
#define LLVM_TEMPORARY "_t"
#define LLVM_GLOBAL_STRING "_global_string"

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
#define LLVM_STRING_TYPE LLVMPointerType(LLVMInt8Type(), 0)

#define LLVM_TRUE_VALUE LLVMConstInt(LLVM_BOOLEAN_TYPE, true, false)
#define LLVM_FALSE_VALUE LLVMConstInt(LLVM_BOOLEAN_TYPE, false, false)

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

    /*
     * The current basic block.
     * Must always set this after repositioning the builder.
     * Must always set this to NULL after adding a terminator instruction.
     */
    LLVMBasicBlockRef block;
} IRState;

static void state_position_builder(IRState* state, LLVMBasicBlockRef block) {
	assert(!state->block);
	LLVMPositionBuilderAtEnd(state->builder, block);
	state->block = block;
}

static bool state_block_open(IRState* state) {
	return (bool) state->block;
}

static void state_close_block(IRState* state) {
	assert(state->block);
	state->block = NULL;
}

// LLVM (TODO: New Module)
static void llvm_function_declaration(LLVMModuleRef, Declaration*);
static LLVMTypeRef llvm_type(Type* type);
static void llvm_return(IRState*, LLVMValueRef);
static LLVMValueRef llvm_string_literal(IRState*, const char*);

// Auxiliary
static LLVMValueRef zerovalue(IRState*, Type*);

// TODO
static void backend_body(IRState*, Body*);
static void backend_declaration(IRState*, Declaration*);
static void backend_definition(IRState*, Definition*);
static int backend_block(IRState*, Block*);
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
		/* function	*/ NULL,
		/* block 	*/ NULL
	};

	// Includes
	// TODO: Should declare something like `extern printf` in the code...
	LLVMTypeRef printf_param_types[] = {LLVM_STRING_TYPE};
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
	assert(body->tag == BODY);
	for (Body* b = body->next; b; b = b->next) {
		switch (b->tag) {
		case BODY_DECLARATION:
			backend_declaration(state, b->declaration);
			continue;
		case BODY_DEFINITION:
			backend_definition(state, b->definition);
			continue;
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

		if (variable->global) { // global
			variable->llvm_value = LLVMAddGlobal(
				state->module,
				llvm_type(variable->type),
				variable->id->name
			);
			backend_expression(state, expression);
			LLVMSetInitializer(variable->llvm_value, expression->llvm_value);
		} else { // scoped variables
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
	case DEFINITION_FUNCTION:
		/* fallthrough */
	case DEFINITION_CONSTRUCTOR: {
		// `backed_declaration` sets state->function internally
		backend_declaration(state, definition->function.declaration);

		state_position_builder(state, LLVMAppendBasicBlock(
			state->function,
			LABEL "_function_entry"
		));

		// Calling alloca and store for parameters
		// TODO: Fix ugly
		for (Declaration* p =
				definition->function.declaration->function.parameters;
				p;
				p = p->next) {
			LLVMValueRef value = p->variable->llvm_value;
			p->variable->llvm_value = LLVMBuildAlloca(
				state->builder,
				llvm_type(p->variable->type),
				p->variable->id->name
			);
			LLVMBuildStore(state->builder, value, p->variable->llvm_value);
		}

		backend_block(state, definition->function.block);

		// implicit return: always returns with the appropriate zero value
		if (state_block_open(state)) {
			llvm_return(
				state,
				zerovalue(
					state,
					definition->function.declaration->function.type
				)
			);
		}
		break;
	}
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

static int backend_block(IRState* state, Block* block) {
	assert(block->tag == BLOCK);

	int counter = 0;
	for (Block* b = block->next; b; b = b->next, counter++) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			backend_declaration(state, b->declaration);
			continue;
		case BLOCK_DEFINITION:
			backend_definition(state, b->definition);
			continue;
		case BLOCK_STATEMENT:
			backend_statement(state, b->statement);
			continue;
		default:
			UNREACHABLE;
		}
	}

	return counter;
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
			llvm_return(state, statement->return_->llvm_value);
		} else {
			llvm_return(state, NULL);
		}
		break;
	case STATEMENT_IF: {
		LLVMBasicBlockRef
			bt = LLVMAppendBasicBlock(state->function, LABEL_IF_TRUE),
			be = LLVMAppendBasicBlock(state->function, LABEL_IF_END);
		backend_condition(state, statement->if_.expression, bt, be);
		// If
		state_position_builder(state, bt);
		backend_block(state, statement->if_.block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// End
		state_position_builder(state, be);
		break;
	}
	case STATEMENT_IF_ELSE: {
		LLVMBasicBlockRef
			bt = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_TRUE),
			bf = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_FALSE),
			be = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_END);
		backend_condition(state, statement->if_else.expression, bt, bf);
		// If
		state_position_builder(state, bt);
		backend_block(state, statement->if_else.if_block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// Else
		state_position_builder(state, bf);
		backend_block(state, statement->if_else.else_block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// End
		state_position_builder(state, be);
		break;
	}
	case STATEMENT_WHILE: {
		LLVMBasicBlockRef
			bw = LLVMAppendBasicBlock(state->function, LABEL_WHILE),
			bl = LLVMAppendBasicBlock(state->function, LABEL_WHILE_LOOP),
			be = LLVMAppendBasicBlock(state->function, LABEL_WHILE_END);
		LLVMBuildBr(state->builder, bw);
		state_close_block(state);
		// While
		state_position_builder(state, bw);
		backend_condition(state, statement->while_.expression, bl, be);
		// Loop
		state_position_builder(state, bl);
		backend_block(state, statement->while_.block);
		LLVMBuildBr(state->builder, bw);
		state_close_block(state);
		// End
		state_position_builder(state, be);
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
		// llvm_value dealed with already
		break;
	case VARIABLE_INDEXED: {
		backend_expression(state, variable->indexed.array);
		backend_expression(state, variable->indexed.index);
		LLVMValueRef index[1] = {variable->indexed.index->llvm_value};
		variable->llvm_value = LLVMBuildGEP(
			state->builder,
			variable->indexed.array->llvm_value,
			index,
			1,
			LLVM_TEMPORARY
		);
		break;
	}
	default:
		UNREACHABLE;
	}
}

static void backend_expression(IRState* state, Expression* expression) {
	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		expression->llvm_value = LLVMConstInt(LLVM_BOOLEAN_TYPE,
			expression->literal_boolean, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->llvm_value = LLVMConstInt(LLVM_INTEGER_TYPE,
			expression->literal_integer, false /* TODO */);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->llvm_value = LLVMConstReal(LLVM_FLOAT_TYPE,
			expression->literal_float);
		break;
	case EXPRESSION_LITERAL_STRING: {
		expression->llvm_value = llvm_string_literal(
			state,
			expression->literal_string
		);
		break;
	}
	case EXPRESSION_VARIABLE:
		backend_variable(state, expression->variable);
		expression->llvm_value = LLVMBuildLoad(
			state->builder,
			expression->variable->llvm_value,
			LLVM_TEMPORARY
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
					expression->unary.expression->llvm_value, LLVM_TEMPORARY);
			} else if (expression->type == float_) {
				expression->llvm_value = LLVMBuildFNeg(state->builder,
					expression->unary.expression->llvm_value, LLVM_TEMPORARY);
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
					e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
			} else if (e->type == float_) { \
				e->llvm_value = ffunc(s->builder, \
					e->binary.left_expression->llvm_value, \
					e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
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
				LLVM_TEMPORARY
			);
		} else if (expression->cast->type == float_ &&
			expression->type == integer_) {
			// Float to Integer
			expression->llvm_value = LLVMBuildFPToSI(
				state->builder,
				expression->cast->llvm_value,
				llvm_type(expression->type),
				LLVM_TEMPORARY
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
		state_position_builder(state, block_true);
		LLVMBuildBr(state->builder, block_phi);
		state_close_block(state);
		state_position_builder(state, block_false);
		LLVMBuildBr(state->builder, block_phi);
		state_close_block(state);
		state_position_builder(state, block_phi);

		LLVMValueRef
			phi = LLVMBuildPhi(state->builder, LLVM_BOOLEAN_TYPE,
				LLVM_TEMPORARY "_phi"),
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
			state_position_builder(state, label);
			backend_condition(state, expression->binary.right_expression,
				lt, lf);
			break;
		}
		case TK_AND: {
			LLVMBasicBlockRef label =
				LLVMAppendBasicBlock(state->function, LABEL "_and");
			backend_condition(state, expression->binary.left_expression,
				label, lf);
			state_position_builder(state, label);
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
					LLVMIntEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else if (l->type == integer_ && r->type == integer_) {
				expression->llvm_value = LLVMBuildICmp(state->builder,
					LLVMIntEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else if (l->type == float_ && r->type == float_) {
				expression->llvm_value = LLVMBuildFCmp(state->builder,
					LLVMRealOEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else {
				UNREACHABLE;
			}
			LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
			state_close_block(state);
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
						r->llvm_value, LLVM_TEMPORARY) \
				: (l->type == float_ && r->type == float_) ? \
					LLVMBuildFCmp(state->builder, ro, l->llvm_value, \
						r->llvm_value, LLVM_TEMPORARY) \
				: (UNREACHABLE, NULL); \
			LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf); \
			state_close_block(state); \
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
		state_close_block(state);
	}
}

static LLVMValueRef backend_function_call(IRState* state, FunctionCall* call) {
	// Arguments
	Expression* e;
	int n = 0;
	for (e = call->arguments; e; e = e->next, n++) {
		backend_expression(state, e);
	}
	int argument_count = n;
	LLVMValueRef arguments[argument_count];
	for (e = call->arguments, n = 0; e; e = e->next, n++) {
		arguments[n] = e->llvm_value;
	}

	switch (call->tag) {
	case FUNCTION_CALL_BASIC: {
		// TODO: Remove this gambiarra
		if (!strcmp(call->basic->name, "print")) {
			return LLVMBuildCall(state->builder, state->printf, arguments,
				argument_count, "");
		}
		break;
	}
	case FUNCTION_CALL_METHOD:
		TODO;
	case FUNCTION_CALL_CONSTRUCTOR:
		if (call->type->tag == TYPE_ARRAY) { // new array
			assert(argument_count == 1);
			return LLVMBuildArrayMalloc(
				/* Builder */		state->builder,
				/* ElementType */	llvm_type(call->type->array),
				/* Size */			arguments[0],
				/* TempName */		LLVM_TEMPORARY
			);
		} else { // monitor constructor
			TODO;
		}
		break;
	default:
		UNREACHABLE;
	}

	/*
	 * OBS: TempName empty string avoids "Instruction has a name, but
	 * provides a void value!" error.
	 */
	return LLVMBuildCall(
		/* Builder */	state->builder,
		/* Function */	call->declaration->llvm_value,
		/* Arguments */	arguments,
		/* NumArgs */	argument_count,
		/* TempName */	""
	);
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
		return LLVMPointerType(llvm_type(type->array), 0);
	default:
		UNREACHABLE;
	}
}

static void llvm_return(IRState* state, LLVMValueRef llvm_value) {
	if (llvm_value) {
		LLVMBuildRet(state->builder, llvm_value);
	} else {
		LLVMBuildRetVoid(state->builder);
	}
	state_close_block(state);
}

// TODO: This functions is wrong, should use LLVMConstString and cast to pointer
static LLVMValueRef llvm_string_literal(IRState* state, const char* string) {
	// Global
	size_t len = strlen(string);
	LLVMValueRef llvm_global = LLVMAddGlobal(
		state->module,
		LLVMArrayType(LLVMInt8Type(), len + 1),
		LLVM_GLOBAL_STRING
	);
	LLVMSetInitializer(llvm_global, LLVMConstString(string, len, false));
	LLVMSetVisibility(llvm_global, LLVMHiddenVisibility);

	// Expression
	return LLVMBuildPointerCast(
		state->builder,
		llvm_global,
		LLVM_STRING_TYPE,
		LLVM_TEMPORARY
	);
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO: Find a better way to write this...
static LLVMValueRef zerovalue(IRState* state, Type* type) {
	switch (type->tag) {
	case TYPE_VOID:
		return NULL;
	case TYPE_ID:
		if (type == boolean_) {
			return LLVMConstInt(LLVM_BOOLEAN_TYPE, false, false);
		}
		if (type == integer_) {
			return LLVMConstInt(LLVM_INTEGER_TYPE, 0, false);
		}
		if (type == float_) {
			return LLVMConstReal(LLVM_FLOAT_TYPE, 0.0);
		}
		if (type == string_) {
			return llvm_string_literal(state, "");
		}
		UNREACHABLE;
	case TYPE_ARRAY:
		return LLVMConstNull(llvm_type(type));
	case TYPE_MONITOR:
		TODO;
	default:
		UNREACHABLE;
	}
}
