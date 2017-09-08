#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove
#include <strings.h> // TODO: Remove

#include <llvm-c/Core.h>

#include "alloc.h"
#include "athreads.h"
#include "ir.h"
#include "ast.h"
#include "parser.h" // for the tokens

/*
 * TODO
 *	- Create macro for declares?
 *	- Create macro for function calls?
 *	- malloc error: mallocs que vc da na main podem ser perdidos quando a main
 *		termina e vc está usando esse malloc em outra thread?
 */

#define UNREACHABLE assert(NULL)	// TODO: Move this and look for assert(NULL)
#define TODO assert(NULL)			// TODO: Remove

// TODO: Move
// Other names
#define NAME_THREAD_ARGUMENTS_STRUCTURE	"_thread_arguments"
#define NAME_SPAWN_FUNCTION				"_spawn_block"

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

// Native types
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// TODO: Move / rename -> see position_builder
static void state_close_block(IRState* state) {
	assert(state->block);
	state->block = NULL;
}

// LLVM (TODO: New Module?)
static LLVMTypeRef llvm_type(Type* type);
static void llvm_return(IRState*, LLVMValueRef);
static LLVMValueRef llvm_string_literal(IRState*, const char*);
static LLVMTypeRef llvm_structure(LLVMTypeRef[], unsigned int, const char*);

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

// TODO: Move to auxiliary area
// returns the mutex lock from a monitor
static LLVMValueRef monitormutex(
	LLVMBuilderRef builder,
	LLVMValueRef monitor) {

	return LLVMBuildLoad(
		builder,
		LLVMBuildStructGEP(builder, monitor, 0, LLVM_TEMPORARY),
		LLVM_TEMPORARY
	);
}

// ==================================================
//
//	Implementation
//
// ==================================================

// TODO: Move / Rename
static void todospawn(IRState* state, FunctionCall* call) {
	// Defining the structure to be used for argument passing
	LLVMTypeRef fields[call->argument_count];
	unsigned int n = 0;
	for (Expression* e = call->arguments; e; e = e->next, n++) {
		fields[n] = llvm_type(e->type);
	}
	LLVMTypeRef type_structure = llvm_structure(
		fields, n, NAME_THREAD_ARGUMENTS_STRUCTURE
	);

	// Allocating memory for the structure
	LLVMValueRef structure = LLVMBuildMalloc(
		state->builder, type_structure, LLVM_TEMPORARY
	);

	// Filling the structure with values from the arguments
	n = 0;
	for (Expression* e = call->arguments; e; e = e->next, n++) {
		backend_expression(state, e);
		LLVMBuildStore(state->builder, e->llvm_value, LLVMBuildStructGEP(
			state->builder, structure, n, LLVM_TEMPORARY_NONE
		));
	}

	// Defining the spawn function
	IRState* spawn_state = ir_state_new(state->module, state->builder);

	spawn_state->function = LLVMAddFunction(
		spawn_state->module, NAME_SPAWN_FUNCTION, _create_start_routine_t
	);
	spawn_state->block = LLVMAppendBasicBlock(
		spawn_state->function, LABEL "spawn_function_entry"
	);
	LLVMPositionBuilderAtEnd(spawn_state->builder, spawn_state->block);

	// Parameters
	LLVMValueRef parameter = LLVMBuildBitCast(
		spawn_state->builder,
		LLVMGetParam(spawn_state->function, 0),
		LLVM_TYPE_POINTER(type_structure),
		LLVM_TEMPORARY
	);
	Definition* p = call->function_definition->function.parameters;
	for (unsigned int n = 0; p; p = p->next, n++) {
		p->variable.variable->llvm_value = LLVMBuildLoad(
			spawn_state->builder,
			LLVMBuildStructGEP(spawn_state->builder, parameter, n, LLVM_TEMPORARY),
			LLVM_TEMPORARY
		);
	}

	// Block
	backend_block(spawn_state, call->function_definition->function.block);

	// TODO: Should free, but weird error (not this...)
	// LLVMBuildFree(state->builder, parameter);
	LLVMBuildRet(
		spawn_state->builder, LLVMConstPointerNull(LLVM_TYPE_POINTER_VOID)
	);

	// Going back to the original state builder position
	LLVMPositionBuilderAtEnd(state->builder, state->block);

	// Calling pthread_create
	ir_pthread_create(
		state->builder,
		spawn_state->function,
			LLVMBuildBitCast(
			state->builder, structure, LLVM_TYPE_POINTER_VOID, LLVM_TEMPORARY
		)
	);

	ir_state_free(spawn_state);
}

LLVMModuleRef backend_compile(AST* ast) {
	// Setup
	__boolean = ast_type_boolean();
	__integer = ast_type_integer();
	__float = ast_type_float();
	__string = ast_type_string();
	__condition_queue = ast_type_condition_queue();

	// LLVM Setup
	IRState* state = ir_state_new(
		LLVMModuleCreateWithName("main.aria"),
		LLVMCreateBuilder()
	);

	// Includes
	ir_setup(state->module);
	ir_pthread_setup(state->module);

	// IR
	for (Definition* d = ast->definitions; d; d = d->next) {
		backend_definition(state, d);
	}

	// Teardown
	ir_state_done(state);
	LLVMModuleRef module = state->module;
	ir_state_free(state);
	return module;
}

static void backend_definition(IRState* state, Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Variable* variable = definition->variable.variable;
		Expression* expression = definition->variable.expression;

		if (variable->global) {
			// Globals
			variable->llvm_value = LLVMAddGlobal(
				state->module,
				llvm_type(variable->type),
				variable->id->name
			);
			backend_expression(state, expression);
			LLVMSetInitializer(variable->llvm_value, expression->llvm_value);
		} else if (!(variable->llvm_structure_index > -1)) {
			// Common scoped variables
			if (variable->value) { // values
				backend_expression(state, expression);
				variable->llvm_value = expression->llvm_value;
			} else { // variables
				variable->llvm_value = LLVMBuildAlloca(
					state->builder,
					llvm_type(variable->type),
					variable->id->name
				);
				if (expression) {
					backend_expression(state, expression);
					LLVMBuildStore(
						state->builder,
						expression->llvm_value,
						variable->llvm_value
					);
				}
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
			state->builder, monitor_type->llvm_type, LLVM_TEMPORARY
		);
		// Allocating memory and initializing the monitor's mutex
		// TODO: free and destroy the mutex one day
		// WORK: New Malloc
		LLVMValueRef mutex = ir_malloc(state->builder, sizeof(pthread_mutex_t));
		ir_pthread_mutex_init(state->builder, mutex);
		LLVMBuildStore(
			state->builder, mutex, LLVMBuildStructGEP(
				state->builder, state->self, 0, LLVM_TEMPORARY
			)
		);		
		// Initializing attributes (that have values)
		Definition* monitor_definitions = monitor_type->monitor.definitions;
		for (Definition* d = monitor_definitions; d; d = d->next) {
			if (d->tag == DEFINITION_VARIABLE) {
				if (d->variable.expression) {
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
		unsigned int n = 1; // attribute count

		for (Definition* d = monitor_definitions; d; d = d->next, n++) {
			if (d->tag != DEFINITION_VARIABLE) {
				continue;
			}
		}

		LLVMTypeRef attributes[n];

		// Monitor's mutex
		attributes[0] = LLVM_TYPE_POINTER_PTHREAD_MUTEX_T;

		// Attributes
		n = 1;
		for (Definition* d = monitor_definitions; d; d = d->next) {
			if (d->tag != DEFINITION_VARIABLE) {
				continue;
			}
			d->variable.variable->llvm_structure_index = n;
			attributes[n++] = llvm_type(d->variable.variable->type);
		}

		type->llvm_type = llvm_structure(attributes, n, type->monitor.id->name);

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
			for (Definition* d = b->definition; d; d = d->next) {
				backend_definition(state, d);
			}
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
	case STATEMENT_WAIT_FOR_IN: {
		LLVMBasicBlockRef
			bw = LLVMAppendBasicBlock(state->function, LABEL_WHILE),
			bl = LLVMAppendBasicBlock(state->function, LABEL_WHILE_LOOP),
			be = LLVMAppendBasicBlock(state->function, LABEL_WHILE_END);
		LLVMBuildBr(state->builder, bw);
		state_close_block(state);
		// While
		position_builder(state, bw);
		backend_condition(state, statement->wait_for_in.condition, be, bl);
		// Loop
		position_builder(state, bl);
		backend_expression(state, statement->wait_for_in.queue);
		LLVMValueRef
			mutex = monitormutex(state->builder, state->self),
			indices[1] = {LLVM_CONSTANT_INTEGER(0)},
			cond = LLVMBuildGEP(
				state->builder,
				statement->wait_for_in.queue->llvm_value,
				indices,
				1,
				LLVM_TEMPORARY
			)
		;
		ir_pthread_cond_wait(state->builder, cond, mutex);
		LLVMBuildBr(state->builder, bw);
		state_close_block(state);
		// End
		position_builder(state, be);
		break;
	}
	case STATEMENT_SIGNAL: {
		backend_expression(state, statement->signal);
		LLVMValueRef
			indices[1] = {LLVM_CONSTANT_INTEGER(0)},
			cond = LLVMBuildGEP(
				state->builder,
				statement->signal->llvm_value,
				indices,
				1,
				LLVM_TEMPORARY
			)
		;
		ir_pthread_cond_signal(state->builder, cond);
		break;
	}
	case STATEMENT_BROADCAST:
		backend_expression(state, statement->broadcast);
		LLVMValueRef
			indices[1] = {LLVM_CONSTANT_INTEGER(0)},
			cond = LLVMBuildGEP(
				state->builder,
				statement->broadcast->llvm_value,
				indices,
				1,
				LLVM_TEMPORARY
			)
		;
		ir_pthread_cond_broadcast(state->builder, cond);
		break;
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
		if (state->block) {
			LLVMBuildBr(state->builder, be);
			state_close_block(state);
		}
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
		if (state->block) {
			LLVMBuildBr(state->builder, be);
			state_close_block(state);
		}
		// Else
		position_builder(state, bf);
		backend_block(state, statement->if_else.else_block);
		if (state->block) {
			LLVMBuildBr(state->builder, be);
			state_close_block(state);
		}
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
		if (state->block) {
			LLVMBuildBr(state->builder, bw);
			state_close_block(state);
		}
		// End
		position_builder(state, be);
		break;
	}
	case STATEMENT_SPAWN: {
		todospawn(state, statement->spawn);
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
		if (variable->llvm_structure_index > -1) { // for attributes
			assert(state->self);
			variable->llvm_value = LLVMBuildStructGEP(
				state->builder,
				state->self,
				variable->llvm_structure_index,
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
		expression->llvm_value = LLVM_CONSTANT_FLOAT(expression->literal_float);
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
		// TODO: Find better way to write this
		if (expression->variable->llvm_structure_index > -1) { // attributes
			expression->llvm_value = LLVMBuildLoad(
				state->builder,
				expression->variable->llvm_value,
				LLVM_TEMPORARY
			);
		} else if (expression->variable->value) { // values
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
			if (expression->type == __integer) {
				expression->llvm_value = LLVMBuildNeg(state->builder,
					expression->unary.expression->llvm_value, LLVM_TEMPORARY);
			} else if (expression->type == __float) {
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
			if (e->type == __integer) { \
				e->llvm_value = ifunc(s->builder, \
					e->binary.left_expression->llvm_value, \
					e->binary.right_expression->llvm_value, LLVM_TEMPORARY); \
			} else if (e->type == __float) { \
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
		if (expression->cast->type == __integer &&
			expression->type == __float) {
			// Integer to Float
			expression->llvm_value = LLVMBuildSIToFP(
				state->builder,
				expression->cast->llvm_value,
				llvm_type(expression->type),
				LLVM_TEMPORARY
			);
		} else if (expression->cast->type == __float &&
			expression->type == __integer) {
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
			phi = LLVMBuildPhi(
				state->builder, LLVM_TYPE_BOOLEAN, LLVM_TEMPORARY_PHI
			),
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
	case EXPRESSION_BINARY: {
		Expression* l = expression->binary.left_expression;
		Expression* r = expression->binary.right_expression;
		LLVMBasicBlockRef label; // used by "or" and "and"

		switch (expression->binary.token) {
		case TK_OR:
			label = LLVMAppendBasicBlock(state->function, LABEL "or");
			backend_condition(state, l, lt, label);
			position_builder(state, label);
			backend_condition(state, r, lt, lf);
			break;
		case TK_AND:
			label = LLVMAppendBasicBlock(state->function, LABEL "and");
			backend_condition(state, l, label, lf);
			position_builder(state, label);
			backend_condition(state, r, lt, lf);
			break;
		case TK_EQUAL:
			backend_expression(state, l);
			backend_expression(state, r);

			// TODO: More readable
			if (l->type == __boolean && r->type == __boolean) {
				expression->llvm_value = LLVMBuildICmp(state->builder,
					LLVMIntEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else if (l->type == __integer && r->type == __integer) {
				expression->llvm_value = LLVMBuildICmp(state->builder,
					LLVMIntEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else if (l->type == __float && r->type == __float) {
				expression->llvm_value = LLVMBuildFCmp(state->builder,
					LLVMRealOEQ, l->llvm_value, r->llvm_value, LLVM_TEMPORARY);
			} else {
				UNREACHABLE;
			}
			LLVMBuildCondBr(state->builder, expression->llvm_value, lt, lf);
			state_close_block(state);
			break;

		// TODO: Rename into/floato based on LLVM parameter names for the types
		// Macro to be used by the [<=, >=, <, >] operations
		#define BINARY_COMPARISSON(into, floato) { \
			backend_expression(state, l); \
			backend_expression(state, r); \
			expression->llvm_value = \
				(l->type == __integer && r->type == __integer) ? \
					LLVMBuildICmp(state->builder, into, l->llvm_value, \
						r->llvm_value, LLVM_TEMPORARY) \
				: (l->type == __float && r->type == __float) ? \
					LLVMBuildFCmp(state->builder, floato, l->llvm_value, \
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
	}
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
	assert(call->argument_count >= 0);
	Expression* e;
	for (e = call->arguments; e; e = e->next) {
		backend_expression(state, e);
	}
	LLVMValueRef args[call->argument_count];
	for (int n = (e = call->arguments, 0); e; e = e->next, n++) {
		args[n] = e->llvm_value;
	}

	switch (call->tag) {
	case FUNCTION_CALL_BASIC:
		// TODO: Remove this gambiarra
		if (!call->function_definition && !strcmp(call->id->name, "print")) {
			return ir_printf(state->builder, args, call->argument_count);
		}
		break;
	case FUNCTION_CALL_METHOD: {
		// Method calls need to acquire the monitor's mutex lock and release it
		LLVMValueRef mutex = monitormutex(
			state->builder, call->arguments->llvm_value
		);

		ir_pthread_mutex_lock(state->builder, mutex);
		LLVMValueRef call_llvm_value = LLVMBuildCall(
			/* Builder */	state->builder,
			/* Function */	call->function_definition->llvm_value,
			/* Arguments */	args,
			/* NumArgs */	call->argument_count,
			/* TempName */	LLVM_TEMPORARY_NONE /* TODO: Look down for OBS */
		);
		ir_pthread_mutex_unlock(state->builder, mutex);

		return call_llvm_value;
	}
	case FUNCTION_CALL_CONSTRUCTOR:
		switch (call->type->tag) {
		case TYPE_ID:
			if (call->type != __condition_queue) {
				UNREACHABLE;
			}
			// ConditionQueue initializer
			LLVMValueRef cond = ir_malloc(
				state->builder, sizeof(pthread_cond_t)
			);
			ir_pthread_cond_init(state->builder, cond);
			return cond;
			break;
		case TYPE_ARRAY: // new array
			assert(call->argument_count == 1);
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
		/* NumArgs */	call->argument_count,
		/* TempName */	LLVM_TEMPORARY_NONE
	);
}

// ==================================================
//
//	LLVM
//
// ==================================================

static LLVMTypeRef llvm_type(Type* type) {
	assert(type);

	switch (type->tag) {
	case TYPE_VOID:
		return LLVM_ARIA_TYPE_VOID;
	case TYPE_ID:
		if (type == __boolean) {
			return LLVM_ARIA_TYPE_BOOLEAN;
		}
		if (type == __integer) {
			return LLVM_ARIA_TYPE_INTEGER;
		}
		if (type == __float) {
			return LLVM_ARIA_TYPE_FLOAT;
		}
		if (type == __string) {
			return LLVM_ARIA_TYPE_STRING;
		}
		if (type == __condition_queue) {
			return LLVM_ARIA_TYPE_CONDITION_QUEUE;
		}
		UNREACHABLE;
	case TYPE_ARRAY:
		return LLVM_ARIA_TYPE_ARRAY(llvm_type(type->array));
	case TYPE_MONITOR:
		return LLVM_ARIA_TYPE_MONITOR(type->llvm_type);
	default:
		UNREACHABLE;
	}
}

// TODO: Rename this: backend_return?
static void llvm_return(IRState* state, LLVMValueRef llvm_value) {
	assert(state->block);

	// The main function always ends with a call to pthred_exit
	if (state->main) {
		ir_pthread_exit(state->builder);
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
		LLVM_ARIA_TYPE_STRING,
		LLVM_TEMPORARY
	);
}

// TODO: Doc
static LLVMTypeRef llvm_structure(LLVMTypeRef fields[], unsigned int length,
	const char* name) {

	LLVMTypeRef type = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
	LLVMStructSetBody(type, fields, length, false); // TODO: Packed?
	return type;
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO: Find a better way to write this...
// TODO: Read about llvm zeroinitalizer
static LLVMValueRef zerovalue(IRState* state, Type* type) {
	switch (type->tag) {
	case TYPE_VOID:
		return NULL;
	case TYPE_ID:
		if (type == __boolean) {
			return LLVM_CONSTANT_FALSE;
		}
		if (type == __integer) {
			return LLVM_CONSTANT_INTEGER(0);
		}
		if (type == __float) {
			return LLVM_CONSTANT_FLOAT(0);
		}
		if (type == __string) {
			return llvm_string_literal(state, "");
		}
		if (type == __condition_queue) {
			return LLVMConstNull(LLVM_ARIA_TYPE_CONDITION_QUEUE);
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
