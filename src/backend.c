#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove
#include <strings.h> // TODO: Remove

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>

#include "alloc.h"
#include "ast.h"
#include "parser.h" // for the tokens

/*
 * TODO
 *	- Calling other methods from the monitor -> trying to reacquire lock
 */

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL)

// TODO: Remove
#define TODO assert(NULL)

// TODO
#define LLVM_TEMPORARY				"_t"
#define LLVM_TEMPORARY_MONITOR_LOCK	LLVM_TEMPORARY "_monitor_lock"

#define LLVM_GLOBAL_STRING "_global_string"

#define LABEL				"label"
#define LABEL_IF_TRUE		LABEL "_if_true"
#define LABEL_IF_END		LABEL "_if_end"
#define LABEL_IF_ELSE_TRUE	LABEL "_if_else_true"
#define LABEL_IF_ELSE_FALSE	LABEL "_if_else_false"
#define LABEL_IF_ELSE_END	LABEL "_if_else_end"
#define LABEL_WHILE			LABEL "_while"
#define LABEL_WHILE_LOOP	LABEL "_while_loop"
#define LABEL_WHILE_END		LABEL "_while_end"

// TODO: LLVM Types / Values

// TODO: Address Space 0 ?
#define LLVM_TYPE_POINTER(t)	LLVMPointerType(t, 0)
#define LLVM_TYPE_MONITOR(t)	LLVM_TYPE_POINTER(t) /* TODO */
// TODO: Rename LLVM_TYPE_POINTER_VOID?
#define LLVM_TYPE_VOID_POINTER	LLVM_TYPE_POINTER(LLVMInt8Type())

// TODO: These are actually LLVM_ARIA_VOID_TYPE and etc
#define LLVM_TYPE_VOID			LLVMVoidType()
#define LLVM_TYPE_BOOLEAN		LLVMIntType(1)
#define LLVM_TYPE_INTEGER		LLVMInt32Type()
#define LLVM_TYPE_FLOAT			LLVMDoubleType()
#define LLVM_TYPE_STRING		LLVM_TYPE_POINTER(LLVMInt8Type())

// TODO: Last argument SignExtend?
#define LLVM_CONSTANT_BOOLEAN(b)	LLVMConstInt(LLVM_TYPE_BOOLEAN, b, false)
#define LLVM_CONSTANT_INTEGER(n)	LLVMConstInt(LLVM_TYPE_INTEGER, n, false)
#define LLVM_CONSTANT_TRUE			LLVM_CONSTANT_BOOLEAN(true)
#define LLVM_CONSTANT_FALSE 		LLVM_CONSTANT_BOOLEAN(false)

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

    // TODO: Gambiarra
    LLVMValueRef self;

    // TODO: Gambiarra
    bool main;
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
static void llvm_function_declaration(LLVMModuleRef, Definition*);
static LLVMTypeRef llvm_type(Type* type);
static void llvm_return(IRState*, LLVMValueRef);
static LLVMValueRef llvm_string_literal(IRState*, const char*);

// Auxiliary
static LLVMValueRef zerovalue(IRState*, Type*);

// TODO
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
//	Threads
//
// ==================================================

// TODO: Move this uptop?
#define NAME_PTHREAD_CREATE			"pthread_create"
#define NAME_PTHREAD_EXIT			"pthread_exit"
#define NAME_PTHREAD_MUTEX_INIT		"pthread_mutex_init"
#define NAME_PTHREAD_MUTEX_LOCK		"pthread_mutex_lock"
#define NAME_PTHREAD_MUTEX_UNLOCK	"pthread_mutex_unlock"

#define NAME_SPAWN_FUNCTION	"_spawn_block"

#define LLVM_TYPE_PTHREAD_T			LLVM_TYPE_VOID_POINTER
#define LLVM_TYPE_PTHREAD_MUTEX_T 	LLVM_TYPE_VOID_POINTER

// TODO: Create macro for declares?

static LLVMTypeRef threads_type_spawn_function(void);

static LLVMValueRef threads_declare_pthread_create(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_exit(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_init(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_lock(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_unlock(LLVMModuleRef);

static void threads_call_pthread_create(IRState*, LLVMValueRef spawn_function);
static void threads_call_pthread_exit(IRState*);
static void threads_call_pthread_mutex_init(IRState*);

static LLVMValueRef threads_define_spawn_function(IRState*, Block*);

// Returns the type the function pthread_create needs to receive as an argument
static LLVMTypeRef threads_type_spawn_function(void) {
	// void *(*start_routine)(void*)
	LLVMTypeRef parameters[1] = {LLVM_TYPE_VOID_POINTER};
	return LLVMFunctionType(
		/* ReturnType	*/ LLVM_TYPE_VOID_POINTER,
		/* ParamTypes	*/ parameters,
		/* ParamCount	*/ 1,
		/* IsVarArg		*/ false
	);
}

static LLVMValueRef threads_declare_pthread_create(LLVMModuleRef module) {
	// int pthread_create(...);
	LLVMTypeRef
		param_types[4] = {
			// pthread_t *thread
			LLVM_TYPE_POINTER(LLVM_TYPE_PTHREAD_T),
			// const pthread_attr_t *attr
			LLVM_TYPE_VOID_POINTER,
			// void *(*start_routine)(void*)
			LLVM_TYPE_POINTER(threads_type_spawn_function()),
			// void *arg
			LLVM_TYPE_VOID_POINTER
		},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 4, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_CREATE, function_ty);
}

static LLVMValueRef threads_declare_pthread_exit(LLVMModuleRef module) {
	// void pthread_exit(void *value_ptr);
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_VOID_POINTER},
		function_ty = LLVMFunctionType(LLVM_TYPE_VOID, param_types, 1, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_EXIT, function_ty);
}

static LLVMValueRef threads_declare_pthread_mutex_init(LLVMModuleRef module) {
	// int pthread_mutex_init(...)
	LLVMTypeRef
		param_types[2] = {
			// pthread_mutex_t *mutex
			LLVM_TYPE_POINTER(LLVM_TYPE_PTHREAD_MUTEX_T),
			// const pthread_mutexattr_t *attr
			LLVM_TYPE_VOID_POINTER
		},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 2, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_MUTEX_INIT, function_ty);
}

static LLVMValueRef threads_declare_pthread_mutex_lock(LLVMModuleRef module) {
	// int pthread_mutex_lock(pthread_mutex_t *mutex)
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_POINTER(LLVM_TYPE_PTHREAD_MUTEX_T)},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 1, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_MUTEX_LOCK, function_ty);
}

static LLVMValueRef threads_declare_pthread_mutex_unlock(LLVMModuleRef module) {
	// int pthread_mutex_unlock(pthread_mutex_t *mutex)
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_POINTER(LLVM_TYPE_PTHREAD_MUTEX_T)},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 1, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_MUTEX_UNLOCK, function_ty);
}

static void threads_call_pthread_create(IRState* state, LLVMValueRef spawn_function) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_CREATE),
		args[4] = {
			// pthread_t *thread // TODO: Isn't this local memory?
			LLVMBuildAlloca(state->builder, LLVM_TYPE_PTHREAD_T, LLVM_TEMPORARY),
			// const pthread_attr_t *attr
			LLVMConstPointerNull(LLVM_TYPE_VOID_POINTER),
			// void *(*start_routine)(void*)
			spawn_function,
			// void *arg
			LLVMConstPointerNull(LLVM_TYPE_VOID_POINTER)
		}
	;
	LLVMBuildCall(state->builder, fn, args, 4, "");
}


static void threads_call_pthread_exit(IRState* state) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_EXIT),
		args[1] = {LLVMConstPointerNull(LLVM_TYPE_VOID_POINTER)}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static LLVMValueRef _pointer_to_lock(IRState* state) {
	LLVMValueRef
		indices[2] = {LLVM_CONSTANT_INTEGER(0), LLVM_CONSTANT_INTEGER(0)}
	;
	return LLVMBuildGEP(
		state->builder, state->self, indices, 2, LLVM_TEMPORARY_MONITOR_LOCK
	);
}

static void threads_call_pthread_mutex_init(IRState* state) {
	assert(state->self);

	LLVMValueRef 		
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_INIT),
		args[2] = {
			// pthread_mutex_t *mutex,
			_pointer_to_lock(state),
			// const pthread_mutexattr_t *attr
			LLVMConstPointerNull(LLVM_TYPE_VOID_POINTER)
		}
	;
	LLVMBuildCall(state->builder, fn, args, 2, "");
}

static void threads_call_pthread_mutex_lock(IRState* state) {
	assert(state->self);

	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_LOCK),
		args[1] = {_pointer_to_lock(state)}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static void threads_call_pthread_mutex_unlock(IRState* state) {
	assert(state->self);

	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_UNLOCK),
		args[1] = {_pointer_to_lock(state)}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static LLVMValueRef threads_define_spawn_function(IRState* state, Block* block) {
	IRState spawn_state = {state->module, state->builder, state->printf, NULL,
		NULL, NULL, false};

	spawn_state.function = LLVMAddFunction(
		/* Module		*/ spawn_state.module,
		/* FunctionName	*/ NAME_SPAWN_FUNCTION,
		/* FunctionType	*/ threads_type_spawn_function()
	);
	spawn_state.block = LLVMAppendBasicBlock(
		/* Function		*/ spawn_state.function,
		/* BlockName	*/ LABEL "_spawn_function_entry"
	);
	LLVMPositionBuilderAtEnd(spawn_state.builder, spawn_state.block);
	backend_block(&spawn_state, block);
	LLVMBuildRet(
		/* Builder		*/ spawn_state.builder,
		/* ReturnValue	*/ LLVMConstPointerNull(LLVM_TYPE_VOID_POINTER)
	);

	// Going back to the original state builder position
	LLVMPositionBuilderAtEnd(state->builder, state->block);

	return spawn_state.function;
}

// ==================================================
//
//	Implementation
//
// ==================================================

LLVMModuleRef backend_compile(AST* ast) {
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
		/* block 	*/ NULL,
		/* self		*/ NULL,
		/* main		*/ false,
	};

	// Includes
	// TODO: Should declare something like `extern printf` in the code...
	// TODO: Can I use LLVMGetNamedFunction instead of a struct "global" value?
	LLVMTypeRef printf_parameter_types[1] = {LLVM_TYPE_STRING};
	LLVMTypeRef type_printf = LLVMFunctionType(
		LLVM_TYPE_INTEGER,
		printf_parameter_types,
		1,
		true
	);
	state.printf = LLVMAddFunction(state.module, "printf", type_printf);

	threads_declare_pthread_create(state.module);
	threads_declare_pthread_exit(state.module);
	threads_declare_pthread_mutex_init(state.module);
	threads_declare_pthread_mutex_lock(state.module);
	threads_declare_pthread_mutex_unlock(state.module);
	// End includes

	for (Definition* d = ast->definitions; d; d = d->next) {
		backend_definition(&state, d);
	}

	// Analysis
	char* error = NULL;
	LLVMVerifyModule(state.module, LLVMAbortProcessAction, &error);
	LLVMDisposeMessage(error);

	// LLVM Teardown
	LLVMDisposeBuilder(state.builder);

	return state.module;
}

static void backend_definition(IRState* state, Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Variable* variable = definition->variable.variable;
		Expression* expression = definition->variable.expression;

		if (variable->global) { // Globals
			variable->llvm_value = LLVMAddGlobal(
				state->module,
				llvm_type(variable->type),
				variable->id->name
			);
			backend_expression(state, expression);
			LLVMSetInitializer(variable->llvm_value, expression->llvm_value);
		} else if (variable->llvm_structure_index) { // Attributes
			// TODO: "else if not variable->llvm_structure_index" below
		} else { // Common scoped variables
			if (variable->value) { // values
				backend_expression(state, expression);
				variable->llvm_value = expression->llvm_value;
			} else { // variables
				variable->llvm_value = LLVMBuildAlloca(
					state->builder,
					llvm_type(variable->type),
					variable->id->name
				);
				backend_expression(state, expression);
				LLVMBuildStore(
					state->builder,
					expression->llvm_value,
					variable->llvm_value
				);
			}
		}
		break;
	}
	case DEFINITION_FUNCTION:
		// TODO: Remove gambiarra
		if (!strcmp(definition->function.id->name, "main")) {
			state->main = true;
		}
		/* fallthrough */
	case DEFINITION_METHOD:
		// TODO: Fix after removing fallthroughs
		if (definition->tag == DEFINITION_METHOD) {
			if (!definition.function->private) {
				threads_call_pthread_mutex_lock(state);
			}
		}
		/* fallthrough */
	case DEFINITION_CONSTRUCTOR: {
		llvm_function_declaration(state->module, definition);
		state->function = definition->llvm_value;

		state_position_builder(state, LLVMAppendBasicBlock(
			state->function,
			LABEL "_function_entry"
		));

		// TODO: Gambiarra
		// Monitor initializer
		if (definition->tag == DEFINITION_CONSTRUCTOR) {
			Type* monitor_type = definition->function.type;

			// Creating the instance
			state->self = LLVMBuildMalloc(
				state->builder,
				monitor_type->llvm_type,
				LLVM_TEMPORARY
			);

			// Initializing the monitor lock
			threads_call_pthread_mutex_init(state);

			// Initializing attributes
			Definition* monitor_definitions = monitor_type->monitor.definitions;
			for (Definition* d = monitor_definitions; d; d = d->next) {
				if (d->tag == DEFINITION_VARIABLE) {
					backend_variable(state, d->variable.variable);
					backend_expression(state, d->variable.expression);
					LLVMBuildStore(
						state->builder,
						d->variable.expression->llvm_value,
						d->variable.variable->llvm_value
					);
				}
			}
		}

		// TODO: Gambiarra
		if (definition->tag == DEFINITION_METHOD) {
			// Self is the first parameter
			state->self =
				definition->function.parameters->variable.variable->llvm_value;
		}

		backend_block(state, definition->function.block);

		// TODO: Gambiarra
		if (definition->tag == DEFINITION_CONSTRUCTOR) {
			if (state_block_open(state)) {
				llvm_return(state, state->self);
			} else {
				state_position_builder(state, LLVMAppendBasicBlock(
					state->function,
					LABEL "_constructor_return"
				));
				llvm_return(state, state->self);
			}
		}

		// Implicit return - always returns with the appropriate zero value
		if (state_block_open(state)) {
			llvm_return(state, zerovalue(state, definition->function.type));
		}
		state->self = NULL; // TODO
		state->main = false; // TODO
		break;
	}
	case DEFINITION_TYPE: {
		assert(definition->type->tag == TYPE_MONITOR);

		Type* type = definition->type;
		Definition* monitor_definitions = type->monitor.definitions;
		int n = 1; // attribute count

		for (Definition* d = monitor_definitions; d; d = d->next, n++) {
			if (d->tag != DEFINITION_VARIABLE) {
				continue;
			}
		}

		LLVMTypeRef attributes[n];

		// Monitor lock
		attributes[0] = LLVM_TYPE_PTHREAD_MUTEX_T;

		// Attributes
		n = 1;
		for (Definition* d = monitor_definitions; d; d = d->next) {
			if (d->tag == DEFINITION_VARIABLE) {
				d->variable.variable->llvm_structure_index =
					LLVM_CONSTANT_INTEGER(n);
				attributes[n++] = llvm_type(d->variable.variable->type);
			}
		}

		type->llvm_type = LLVMStructCreateNamed(
			LLVMGetGlobalContext(),
			type->monitor.id->name
		);
		LLVMStructSetBody(type->llvm_type, attributes, n, false); // TODO: Packed?

		for (Definition* d = monitor_definitions; d; d = d->next) {
			switch (d->tag) {
			case DEFINITION_METHOD:
				/* fallthrough */
			case DEFINITION_CONSTRUCTOR:
				backend_definition(state, d);
				break;
			default:
				continue;
			}
		}

		break;
	}
	default:
		UNREACHABLE;
	}
}

static void backend_block(IRState* state, Block* block) {
	assert(block->tag == BLOCK);
	for (Block* b = block->next; b; b = b->next) {
		switch (b->tag) {
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
	case STATEMENT_SPAWN: {
		LLVMValueRef spawn_function = threads_define_spawn_function(
			state,
			statement->spawn
		);
		threads_call_pthread_create(state, spawn_function);
		break;
	}
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
		// llvm_value dealt with already, unless if attribute
		if (variable->llvm_structure_index) {
			assert(state->self);

			LLVMValueRef indexes[2] = {
				LLVM_CONSTANT_INTEGER(0),
				variable->llvm_structure_index
			};
			variable->llvm_value = LLVMBuildGEP(
				state->builder,
				state->self,
				indexes,
				2,
				LLVM_TEMPORARY
			);
		}
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
		expression->llvm_value =
			LLVM_CONSTANT_BOOLEAN(expression->literal_boolean);
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->llvm_value =
			LLVM_CONSTANT_INTEGER(expression->literal_integer);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->llvm_value = LLVMConstReal(LLVM_TYPE_FLOAT,
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
		if (expression->variable->value) { // values
			expression->llvm_value = expression->variable->llvm_value;
		} else { // variables
			expression->llvm_value = LLVMBuildLoad(
				state->builder,
				expression->variable->llvm_value,
				LLVM_TEMPORARY
			);
		}
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
			phi = LLVMBuildPhi(state->builder, LLVM_TYPE_BOOLEAN,
				LLVM_TEMPORARY "_phi"),
			incoming_values[2] = {LLVM_CONSTANT_TRUE, LLVM_CONSTANT_FALSE};
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
	assert(call->arguments_count > -1);
	Expression* e;
	for (e = call->arguments; e; e = e->next) {
		backend_expression(state, e);
	}
	LLVMValueRef arguments[call->arguments_count];
	for (int n = (e = call->arguments, 0); e; e = e->next, n++) {
		arguments[n] = e->llvm_value;
	}

	switch (call->tag) {
	case FUNCTION_CALL_BASIC:
		// TODO: Remove this gambiarra
		if (!call->function_definition && !strcmp(call->id->name, "print")) {
			return LLVMBuildCall(state->builder, state->printf, arguments,
				call->arguments_count, "");
		}
		break;
	case FUNCTION_CALL_METHOD:
		break;
	case FUNCTION_CALL_CONSTRUCTOR:
		switch (call->type->tag) {
		case TYPE_ARRAY: // new array
			assert(call->arguments_count == 1);
			return LLVMBuildArrayMalloc(
				/* Builder */		state->builder,
				/* ElementType */	llvm_type(call->type->array),
				/* Size */			arguments[0],
				/* TempName */		LLVM_TEMPORARY
			);
		case TYPE_MONITOR: // monitor constructor
			break;
		default:
			UNREACHABLE;
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
		/* Function */	call->function_definition->llvm_value,
		/* Arguments */	arguments,
		/* NumArgs */	call->arguments_count,
		/* TempName */	""
	);
}

// ==================================================
//
//	LLVM
//
// ==================================================

// TODO: Remove?
// sets the value reference for the function prototype inside declaration
static void llvm_function_declaration(LLVMModuleRef module,
	Definition* definition) {

	assert(definition->function.id);
	assert(definition->function.type);
	
	// Counting the number of parameters
	Definition* parameter = definition->function.parameters;
	unsigned int paramCount = 0;
	for (Definition* p = parameter; p; p = p->next, paramCount++);

	// Creating a list of parameters
	LLVMTypeRef paramTypes[paramCount]; // TODO: Won't this be destroyed?
	parameter = definition->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		paramTypes[i] = llvm_type(parameter->variable.variable->type);
	}

	// Creating the function prototype and setting it as the current function
	definition->llvm_value = LLVMAddFunction(
		/* Module		*/ module,
		/* FunctionName	*/ definition->function.id->name,
		/* FunctionRef	*/ LLVMFunctionType(
			/* ReturnType	*/ llvm_type(definition->function.type),
			/* ParamTypes	*/ paramTypes,
			/* ParamCount	*/ paramCount,
			/* IsVarArg		*/ false
		)
	);

	// Setting the names for the parameters
	parameter = definition->function.parameters;
	for (int i = 0; parameter; parameter = parameter->next, i++) {
		parameter->variable.variable->llvm_value = LLVMGetParam(
			definition->llvm_value,
			i
		);
		LLVMSetValueName(
			parameter->variable.variable->llvm_value,
			parameter->variable.variable->id->name
		);
	}
}

static LLVMTypeRef llvm_type(Type* type) {
	switch (type->tag) {
	case TYPE_VOID:
		return LLVM_TYPE_VOID;
	case TYPE_ID:
		if (type == boolean_) {
			return LLVM_TYPE_BOOLEAN;
		}
		if (type == integer_) {
			return LLVM_TYPE_INTEGER;
		}
		if (type == float_) {
			return LLVM_TYPE_FLOAT;
		}
		if (type == string_) {
			return LLVM_TYPE_STRING;
		}
		UNREACHABLE;
	case TYPE_ARRAY:
		return LLVM_TYPE_POINTER(llvm_type(type->array));
	case TYPE_MONITOR:
		return LLVM_TYPE_MONITOR(type->llvm_type);
	default:
		UNREACHABLE;
	}
}

static void llvm_return(IRState* state, LLVMValueRef llvm_value) {
	if (state->main) {
		threads_call_pthread_exit(state);
	}

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
		LLVM_TYPE_STRING,
		LLVM_TEMPORARY
	);
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO: Find a better way to write this...
// TODO: Check for llvm zeroinitalizer
static LLVMValueRef zerovalue(IRState* state, Type* type) {
	switch (type->tag) {
	case TYPE_VOID:
		return NULL;
	case TYPE_ID:
		if (type == boolean_) {
			return LLVM_CONSTANT_FALSE;
		}
		if (type == integer_) {
			return LLVM_CONSTANT_INTEGER(0);
		}
		if (type == float_) {
			return LLVMConstReal(LLVM_TYPE_FLOAT, 0.0);
		}
		if (type == string_) {
			return llvm_string_literal(state, "");
		}
		UNREACHABLE;
	case TYPE_ARRAY:
		return LLVMConstNull(llvm_type(type));
	case TYPE_MONITOR:
		return LLVMConstNull(llvm_type(type));
	default:
		UNREACHABLE;
	}
}
