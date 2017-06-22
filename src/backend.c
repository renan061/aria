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
 *	- Locking on monitor method call -> Can this be a problem?
 *		- Check when the lock is lost due to condition variables.
 *	- Create macro for declares?
 *	- Create macro for function calls?
 */

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL)

// TODO: Remove
#define TODO assert(NULL)

// Names of declared functions from external sources
#define NAME_PRINTF					"printf"
#define NAME_PTHREAD_CREATE			"pthread_create"
#define NAME_PTHREAD_EXIT			"pthread_exit"
#define NAME_PTHREAD_MUTEX_INIT		"pthread_mutex_init"
#define NAME_PTHREAD_MUTEX_LOCK		"pthread_mutex_lock"
#define NAME_PTHREAD_MUTEX_UNLOCK	"pthread_mutex_unlock"

// TODO
#define LLVM_TEMPORARY				"_t"
#define LLVM_TEMPORARY_MONITOR_LOCK	LLVM_TEMPORARY "_monitor_lock"

#define LLVM_GLOBAL_STRING "_global_string"

#define LABEL					"label_"
#define LABEL_FUNCTION_ENTRY	LABEL "function_entry"
#define LABEL_IF_TRUE			LABEL "if_true"
#define LABEL_IF_END			LABEL "if_end"
#define LABEL_IF_ELSE_TRUE		LABEL "if_else_true"
#define LABEL_IF_ELSE_FALSE		LABEL "if_else_false"
#define LABEL_IF_ELSE_END		LABEL "if_else_end"
#define LABEL_WHILE				LABEL "while"
#define LABEL_WHILE_LOOP		LABEL "while_loop"
#define LABEL_WHILE_END			LABEL "while_end"

// TODO: LLVM Types / Values

// TODO: Address Space 0 ?
#define LLVM_TYPE_POINTER(t)	LLVMPointerType(t, 0)
// TODO: Monitor type is pointer to monitor struct type? Think about it.
#define LLVM_TYPE_MONITOR(m)	LLVM_TYPE_POINTER(m)
#define LLVM_TYPE_POINTER_VOID	LLVM_TYPE_POINTER(LLVMInt8Type())

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
    LLVMValueRef function; // current function

    /*
     * The current basic block.
     * Must always set this after repositioning the builder.
     * Must always set this to NULL after adding a terminator instruction.
     */
    LLVMBasicBlockRef block;

    // TODO: Gambiarra
    LLVMValueRef self;

    // TODO: Gambiarra?
    bool main;			// if inside the main function
    bool initializer;	// if inside an initializer
} IRState;

const IRState NEW_IR_STATE = {
	.module			= NULL,
	.builder		= NULL,
	.function		= NULL,
	.block			= NULL,
	.self			= NULL,
	.main			= false,
	.initializer	= false,
};

static void state_close_block(IRState* state) {
	assert(state->block);
	state->block = NULL;
}

// LLVM (TODO: New Module?)
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
//	TODO: Misc
//
// ==================================================

static LLVMValueRef declare_printf(LLVMModuleRef module) {
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_STRING},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 1, true)
	;
	return LLVMAddFunction(module, NAME_PRINTF, function_ty);
}

static LLVMValueRef declare_function(LLVMModuleRef m, Definition* function) {
	assert(function->function.id);
	assert(function->function.type);
	
	Definition* p; // parameter

	// Counting the number of parameters
	unsigned int param_count = 0;
	for (p = function->function.parameters; p; p = p->next, param_count++);

	// Creating a list of parameter types
	LLVMTypeRef param_types[param_count]; // TODO: Isn't this local memory?
	p = function->function.parameters;
	for (unsigned int i = 0; p; p = p->next, i++) {
		param_types[i] = llvm_type(p->variable.variable->type);
	}

	// Creating the function prototype and setting it as the current function
	LLVMValueRef fn = LLVMAddFunction(
		/* Module		*/ m,
		/* FunctionName	*/ function->function.id->name,
		/* FunctionRef	*/ LLVMFunctionType(
			/* ReturnType	*/ llvm_type(function->function.type),
			/* ParamTypes	*/ param_types,
			/* ParamCount	*/ param_count,
			/* IsVarArg		*/ false
		)
	);

	// Giving values to the parameters
	p = function->function.parameters;
	for (unsigned int i = 0; p; p = p->next, i++) {
		p->variable.variable->llvm_value = LLVMGetParam(fn, i);
	}

	return fn;
}

static void position_builder(IRState* state, LLVMBasicBlockRef block) {
	assert(!state->block);
	LLVMPositionBuilderAtEnd(state->builder, block);
	state->block = block;
}

// ==================================================
//
//	TODO: Threads
//
// ==================================================

#define NAME_SPAWN_FUNCTION	"_spawn_block"

#define LLVM_TYPE_PTHREAD_T			LLVM_TYPE_POINTER_VOID
#define LLVM_TYPE_PTHREAD_MUTEX_T 	LLVM_TYPE_POINTER_VOID

// TODO: Docs
static LLVMTypeRef threads_type_spawn_function(void);
static LLVMValueRef threads_define_spawn_function(IRState*, Block*);
static LLVMValueRef threads_monitor_lock(LLVMBuilderRef, LLVMValueRef);

// TODO: Docs
static LLVMValueRef threads_declare_pthread_create(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_exit(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_init(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_lock(LLVMModuleRef);
static LLVMValueRef threads_declare_pthread_mutex_unlock(LLVMModuleRef);

// TODO: Docs
static void threads_call_pthread_create(IRState*, LLVMValueRef spawn_function);
static void threads_call_pthread_exit(IRState*);
static void threads_call_pthread_mutex_init(IRState*, LLVMValueRef);
static void threads_call_pthread_mutex_lock(IRState*, LLVMValueRef);
static void threads_call_pthread_mutex_unlock(IRState*, LLVMValueRef);


// Returns the type the function pthread_create needs to receive as an argument
static LLVMTypeRef threads_type_spawn_function(void) {
	// void *(*start_routine)(void*)
	LLVMTypeRef parameters[1] = {LLVM_TYPE_POINTER_VOID};
	return LLVMFunctionType(
		/* ReturnType	*/ LLVM_TYPE_POINTER_VOID,
		/* ParamTypes	*/ parameters,
		/* ParamCount	*/ 1,
		/* IsVarArg		*/ false
	);
}

// Returns the lock from a monitor
static LLVMValueRef threads_monitor_lock(LLVMBuilderRef b, LLVMValueRef m) {
	LLVMValueRef
		indices[2] = {LLVM_CONSTANT_INTEGER(0), LLVM_CONSTANT_INTEGER(0)}
	;
	return LLVMBuildGEP(b, m, indices, 2, LLVM_TEMPORARY_MONITOR_LOCK);
}

static LLVMValueRef threads_declare_pthread_create(LLVMModuleRef module) {
	// int pthread_create(...);
	LLVMTypeRef
		param_types[4] = {
			// pthread_t *thread
			LLVM_TYPE_POINTER(LLVM_TYPE_PTHREAD_T),
			// const pthread_attr_t *attr
			LLVM_TYPE_POINTER_VOID,
			// void *(*start_routine)(void*)
			LLVM_TYPE_POINTER(threads_type_spawn_function()),
			// void *arg
			LLVM_TYPE_POINTER_VOID
		},
		function_ty = LLVMFunctionType(LLVM_TYPE_INTEGER, param_types, 4, false)
	;
	return LLVMAddFunction(module, NAME_PTHREAD_CREATE, function_ty);
}

static LLVMValueRef threads_declare_pthread_exit(LLVMModuleRef module) {
	// void pthread_exit(void *value_ptr);
	LLVMTypeRef
		param_types[1] = {LLVM_TYPE_POINTER_VOID},
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
			LLVM_TYPE_POINTER_VOID
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
			LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID),
			// void *(*start_routine)(void*)
			spawn_function,
			// void *arg
			LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
		}
	;
	LLVMBuildCall(state->builder, fn, args, 4, "");
}


static void threads_call_pthread_exit(IRState* state) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_EXIT),
		args[1] = {LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static void threads_call_pthread_mutex_init(IRState* state, LLVMValueRef lock) {
	LLVMValueRef 		
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_INIT),
		args[2] = {
			// pthread_mutex_t *mutex,
			lock,
			// const pthread_mutexattr_t *attr
			LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
		}
	;
	LLVMBuildCall(state->builder, fn, args, 2, "");
}

static void threads_call_pthread_mutex_lock(IRState* state, LLVMValueRef lock) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_LOCK),
		args[1] = {lock}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static void threads_call_pthread_mutex_unlock(IRState* state, LLVMValueRef lock) {
	LLVMValueRef
		fn = LLVMGetNamedFunction(state->module, NAME_PTHREAD_MUTEX_UNLOCK),
		args[1] = {lock}
	;
	LLVMBuildCall(state->builder, fn, args, 1, "");
}

static LLVMValueRef threads_define_spawn_function(IRState* state, Block* block) {
	IRState spawn_state = NEW_IR_STATE;
	spawn_state.module = state->module;
	spawn_state.builder = state->builder;

	spawn_state.function = LLVMAddFunction(
		/* Module		*/ spawn_state.module,
		/* FunctionName	*/ NAME_SPAWN_FUNCTION,
		/* FunctionType	*/ threads_type_spawn_function()
	);
	spawn_state.block = LLVMAppendBasicBlock(
		/* Function		*/ spawn_state.function,
		/* BlockName	*/ LABEL "spawn_function_entry"
	);
	LLVMPositionBuilderAtEnd(spawn_state.builder, spawn_state.block);
	backend_block(&spawn_state, block);
	LLVMBuildRet(
		/* Builder		*/ spawn_state.builder,
		/* ReturnValue	*/ LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
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
	IRState state = NEW_IR_STATE;
	state.module = LLVMModuleCreateWithName("main.aria");
	state.builder = LLVMCreateBuilder();

	// Includes
	declare_printf(state.module);
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
		definition->llvm_value = declare_function(state->module, definition);
		state->function = definition->llvm_value;
		position_builder(state, LLVMAppendBasicBlock(
			state->function,
			LABEL_FUNCTION_ENTRY
		));

		// TODO: Remove gambiarra
		if (!strcmp(definition->function.id->name, "main")) {
			state->main = true;
		}

		backend_block(state, definition->function.block);

		// Implicit return
		if (state->block) {
			llvm_return(state, zerovalue(state, definition->function.type));
		}
		state->main = false; // TODO
		break;
	case DEFINITION_METHOD:
		definition->llvm_value = declare_function(state->module, definition);
		state->function = definition->llvm_value;
		position_builder(state, LLVMAppendBasicBlock(
			state->function,
			LABEL_FUNCTION_ENTRY
		));

		// Self is the first parameter
		state->self = definition->function.parameters->variable.variable->llvm_value;

		backend_block(state, definition->function.block);

		// Implicit return
		if (state->block) {
			llvm_return(state, zerovalue(state, definition->function.type));
		}

		state->self = NULL;
		break;
	case DEFINITION_CONSTRUCTOR: {
		state->initializer = true;

		definition->llvm_value = declare_function(state->module, definition);
		state->function = definition->llvm_value;
		position_builder(state, LLVMAppendBasicBlock(
			state->function,
			LABEL_FUNCTION_ENTRY
		));

		// Monitor initializer
		Type* monitor_type = definition->function.type;
		// Creating the instance
		state->self = LLVMBuildMalloc(
			state->builder,
			monitor_type->llvm_type,
			LLVM_TEMPORARY
		);
		// Initializing the monitor lock (TODO: destroy the lock one day)
		LLVMValueRef lock = threads_monitor_lock(state->builder, state->self);
		threads_call_pthread_mutex_init(state, lock);
		// Initializing attributes (that have values)
		Definition* monitor_definitions = monitor_type->monitor.definitions;
		for (Definition* d = monitor_definitions; d; d = d->next) {
			if (d->tag == DEFINITION_VARIABLE && d->variable.expression) {
				backend_variable(state, d->variable.variable);
				backend_expression(state, d->variable.expression);
				LLVMBuildStore(
					state->builder,
					d->variable.expression->llvm_value,
					d->variable.variable->llvm_value
				);
			}
		}

		backend_block(state, definition->function.block);

		if (state->block) {
			llvm_return(state, state->self);
		}

		state->self = NULL;
		state->initializer = false;
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
		position_builder(state, bt);
		backend_block(state, statement->if_.block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// End
		position_builder(state, be);
		break;
	}
	case STATEMENT_IF_ELSE: {
		LLVMBasicBlockRef
			bt = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_TRUE),
			bf = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_FALSE),
			be = LLVMAppendBasicBlock(state->function, LABEL_IF_ELSE_END);
		backend_condition(state, statement->if_else.expression, bt, bf);
		// If
		position_builder(state, bt);
		backend_block(state, statement->if_else.if_block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// Else
		position_builder(state, bf);
		backend_block(state, statement->if_else.else_block);
		LLVMBuildBr(state->builder, be);
		state_close_block(state);
		// End
		position_builder(state, be);
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
		position_builder(state, bw);
		backend_condition(state, statement->while_.expression, bl, be);
		// Loop
		position_builder(state, bl);
		backend_block(state, statement->while_.block);
		LLVMBuildBr(state->builder, bw);
		state_close_block(state);
		// End
		position_builder(state, be);
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
			block_true	= LLVMAppendBasicBlock(state->function, LABEL "a"),
			block_false	= LLVMAppendBasicBlock(state->function, LABEL "b"),
			block_phi	= LLVMAppendBasicBlock(state->function, LABEL "phi")
		;

		backend_condition(state, expression, block_true, block_false);
		position_builder(state, block_true);
		LLVMBuildBr(state->builder, block_phi);
		state_close_block(state);
		position_builder(state, block_false);
		LLVMBuildBr(state->builder, block_phi);
		state_close_block(state);
		position_builder(state, block_phi);

		LLVMValueRef
			phi = LLVMBuildPhi(state->builder, LLVM_TYPE_BOOLEAN,
				LLVM_TEMPORARY "_phi"),
			incoming_values[2] = {LLVM_CONSTANT_TRUE, LLVM_CONSTANT_FALSE}
		;
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
				LLVMAppendBasicBlock(state->function, LABEL "or");
			backend_condition(state, expression->binary.left_expression,
				lt, label);
			position_builder(state, label);
			backend_condition(state, expression->binary.right_expression,
				lt, lf);
			break;
		}
		case TK_AND: {
			LLVMBasicBlockRef label =
				LLVMAppendBasicBlock(state->function, LABEL "and");
			backend_condition(state, expression->binary.left_expression,
				label, lf);
			position_builder(state, label);
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
	assert(call->arguments_count >= 0);
	Expression* e;
	for (e = call->arguments; e; e = e->next) {
		backend_expression(state, e);
	}
	LLVMValueRef args[call->arguments_count];
	for (int n = (e = call->arguments, 0); e; e = e->next, n++) {
		args[n] = e->llvm_value;
	}

	switch (call->tag) {
	case FUNCTION_CALL_BASIC:
		// TODO: Remove this gambiarra
		if (!call->function_definition && !strcmp(call->id->name, "print")) {
			return LLVMBuildCall(
				state->builder,
				LLVMGetNamedFunction(state->module, NAME_PRINTF),
				args,
				call->arguments_count,
				""
			);
		}
		break;
	case FUNCTION_CALL_METHOD: {
		// Method calls need to acquire the monitor's mutex lock and release it
		LLVMValueRef lock = threads_monitor_lock(
			state->builder, call->arguments->llvm_value
		);

		threads_call_pthread_mutex_lock(state, lock);
		LLVMValueRef call_llvm_value = LLVMBuildCall(
			/* Builder */	state->builder,
			/* Function */	call->function_definition->llvm_value,
			/* Arguments */	args,
			/* NumArgs */	call->arguments_count,
			/* TempName */	"" /* TODO: Look down for OBS */
		);
		threads_call_pthread_mutex_unlock(state, lock);

		return call_llvm_value;
	}
	case FUNCTION_CALL_CONSTRUCTOR:
		switch (call->type->tag) {
		case TYPE_ARRAY: // new array
			assert(call->arguments_count == 1);
			return LLVMBuildArrayMalloc(
				/* Builder */		state->builder,
				/* ElementType */	llvm_type(call->type->array),
				/* Size */			args[0],
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

	// TODO: Make macro for this llvm call and repeat it instead of breaking

	/*
	 * OBS: TempName empty string avoids "Instruction has a name, but
	 * provides a void value!" error.
	 */
	return LLVMBuildCall(
		/* Builder */	state->builder,
		/* Function */	call->function_definition->llvm_value,
		/* Arguments */	args,
		/* NumArgs */	call->arguments_count,
		/* TempName */	""
	);
}

// ==================================================
//
//	LLVM
//
// ==================================================

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

// TODO: Rename this: backend_return?
static void llvm_return(IRState* state, LLVMValueRef llvm_value) {
	assert(state->block);

	// The main function always ends with a call to pthred_exit
	if (state->main) {
		threads_call_pthread_exit(state);
		LLVMBuildRetVoid(state->builder);
		state_close_block(state);
		return;
	}

	// Initializers always returns the 'self' reference
	if (state->initializer) {
		assert(state->self);
		LLVMBuildRet(state->builder, state->self);
		state_close_block(state);
		return;
	}

	// Return for normal functions and methods
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
