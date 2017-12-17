/*
 * TODO
 *
 *	- Immutable Monitor1 is wrong (TODO: being filtered by the parser)
 *	- Return statements with no expressions should return the 'nil' expression
 *
 */
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h> // TODO: Remove
#include <string.h>

#include "alloc.h"
#include "ast.h"
#include "errs.h"
#include "parser.h" // for the tokens
#include "symtable.h"

// TODO: Look for assert(NULL) in the code

// TODO: Also check if TODOERR errors have matching tests
#define TODOERR(line, err) \
	printf("line %d:\n\tsemantic error: %s\n", line, err); exit(1); \

// TODO
#define PREPEND(Type, head, element) \
	{ \
		Type* _temporary = head; \
		head = element; \
		head->next = _temporary; \
	} \

// Stores important information about the current state of the semantic analysis
typedef struct SemanticState {
	// Symbol table for definitions
	SymbolTable* table;

	// <type-monitor> if currently analysing a monitor's body, <NULL> otherwise
	Type* monitor;

	// <type> if currently analysing a function's body, <NULL> otherwise
	Type* return_;

	// <true> if currently analysing a monitor's initializer, <false> otherwise
	bool initializer;

	// The spawn scope if currently analysing a spawn statement block, NULL
	// otherwise
	struct {
		Scope* scope;
		FunctionCall* function_call;
	} spawn;
} SemanticState;

// Native types
static Type* __void;
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// Functions that analyse the abstract syntax tree recursively
static void sem_definition(SemanticState*, Definition*);
static void sem_block(SemanticState*, Block*);
static void sem_statement(SemanticState*, Statement*);
static void sem_variable(SemanticState*, Variable**);
static void sem_expression(SemanticState*, Expression*);
static void sem_function_call(SemanticState*, FunctionCall*);

// Auxiliary functions that deal with type analysis
static void linktype(SymbolTable*, Type**);
static void assignment(Variable*, Expression**);
static void typecheck1(Type*, Expression**);
static Type* typecheck2(Expression**, Expression**);
static bool indextype(Type*);
static bool numerictype(Type*);
static bool conditiontype(Type*);
static bool equatabletype(Type*);
static bool safetype(Type*);

// TODO
// static void freetype(Type*);
static void freetypeid(Type*);
// static void freetypearray(Type*);

// Auxiliary functions that deal with errors
static void err_redeclaration(Id*);
static void err_unkown_type_name(Id*);
static void err_invalid_condition_type(Expression*);
static void err_type(Line, Type*, Type*);
static void err_assignment_value(Statement*);
static void err_return_inside_spawn(Line);
static void err_return_initializer(Line);
static void err_return_void(Line, Type*);
static void err_variable_unknown(Id*);
static void err_variable_misuse(Id*);
static void err_variable_array_type(Variable*);
static void err_variable_array_index_type(Variable*);
static void err_function_call_unknown(Id*);
static void err_function_call_misuse(Id*);
static void err_function_call_array_constructor(Line, unsigned int);
static void err_function_call_no_constructor(Line, Id*);
static void err_function_call_few_args(Line);
static void err_function_call_excess_args(Line);
static void err_function_call_no_monitor(Line);
static void err_function_call_private(Line, Id*, Type*);
static void err_function_call_no_method(Line, Type*, Id*);
static void err_monitor_statements(Line, const char*);
static void err_monitor_statements_constructor(Line, const char*);
static void err_monitor_function_type(Line);
static void err_spawn_variable(Line);
static void err_spawn_unsafe(Line);

// TODO: Experimental
typedef enum ErrorType {
	ERR_EXPRESSION_MINUS = 0,
	ERR_EXPRESSION_NOT,
	ERR_EXPRESSION_LEFT,
	ERR_EXPRESSION_RIGHT,
	ERR_EXPRESSION_LEFT_EQUAL,
	ERR_EXPRESSION_RIGHT_EQUAL,
	ERR_EXPRESSION_TYPECHECK_EQUAL
} ErrorType;
static void err_expression(ErrorType, Expression*);

// ==================================================
//
//	Exported
//
// ==================================================

/*
 * TODO
 */
void sem_analyse(AST* ast) {
	// Setup
	__void = ast_type_void();
	__boolean = ast_type_boolean();
	__integer = ast_type_integer();
	__float = ast_type_float();
	__string = ast_type_string();
	__condition_queue = ast_type_condition_queue();

	Definition* def_boolean = ast_definition_type(__boolean);
	Definition* def_integer = ast_definition_type(__integer);
	Definition* def_float = ast_definition_type(__float);
	Definition* def_string = ast_definition_type(__string);
	Definition* def_condition_queue = ast_definition_type(__condition_queue);

	SemanticState state = {
		/* symbol table			*/ symtable_new(),
		/* current monitor		*/ NULL,
		/* return type			*/ NULL,
		/* inside initializer	*/ false,
	};
	state.spawn.scope = NULL;
	state.spawn.function_call = NULL;

	// Analysis
	symtable_enter_scope(state.table);
	symtable_insert(state.table, def_boolean);
	symtable_insert(state.table, def_integer);
	symtable_insert(state.table, def_float);
	symtable_insert(state.table, def_string);
	symtable_insert(state.table, def_condition_queue);
	for (Definition* d = ast->definitions; d; d = d->next) {
		sem_definition(&state, d);
	}
	symtable_leave_scope(state.table);

	// Teardown
	symtable_free(state.table);
	free(def_boolean);
	free(def_integer);
	free(def_float);
	free(def_string);
	free(def_condition_queue);

	assert(!state.monitor);
	assert(!state.return_);
	assert(!state.initializer);
	assert(!state.spawn.scope);
	assert(!state.spawn.function_call);
}

// ==================================================
//
//	Implementation
//
// ==================================================

// TODO
static Variable* astself(Line line) {
	static const char* keyword_self = "self";
	Variable* variable = ast_variable_id(ast_id(line, keyword_self));
	variable->global = false;
	variable->value = true;
	return variable;
}

static void sem_definition(SemanticState* state, Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Variable* variable = definition->variable.variable;
		if (!symtable_insert(state->table, definition)) {
			err_redeclaration(variable->id);
		}
		if (variable->type) { // For non-inferred types
			linktype(state->table, &variable->type);
		}
		if (definition->variable.expression) {
			sem_expression(state, definition->variable.expression);
			assignment(variable, &definition->variable.expression);
			if (variable->global && !safetype(variable->type)) {
				TODOERR(
					variable->line,
					"global variables need to have Immutable types"
				);
			}
		}
		break;
	}
	case DEFINITION_FUNCTION:
		assert(!definition->function.private);
		if (!symtable_insert(state->table, definition)) {
			err_redeclaration(definition->function.id);
		}
		linktype(state->table, &definition->function.type);

	DEFAULT_DEFINITION_FUNCTION: {
		// Parameters
		symtable_enter_scope(state->table);
		for (Definition* p = definition->function.parameters; p; p = p->next) {
			sem_definition(state, p);
			if (state->monitor && !safetype(p->variable.variable->type)) {
				TODOERR(
					p->variable.variable->line,
					"parameters of monitor functions need to have safe types"
				);
			}
		}

		// Block
		state->return_ = definition->function.type;
		sem_block(state, definition->function.block);
		state->return_ = NULL;
		symtable_leave_scope(state->table);
		state->initializer = false;
		break;
	}
	case DEFINITION_METHOD: {
		assert(state->monitor);
		
		// TODO: Take line (-1) from definition
		Variable* variable = astself(-1);
		variable->type = ast_type_id(ast_id(
			-1,
			state->monitor->monitor.id->name)
		);

		PREPEND(Definition, definition->function.parameters,
			ast_definition_variable(variable, NULL));

		if (!symtable_insert(state->table, definition)) {
			err_redeclaration(definition->function.id);
		}
		if (!safetype(definition->function.type)) {
			err_monitor_function_type(definition->function.id->line);
		}
		linktype(state->table, &definition->function.type);

		goto DEFAULT_DEFINITION_FUNCTION;
	}
	case DEFINITION_CONSTRUCTOR:
		assert(state->monitor);
		state->initializer = true;
		definition->function.id = state->monitor->monitor.id;
		definition->function.type = state->monitor;	
		goto DEFAULT_DEFINITION_FUNCTION;
	case DEFINITION_TYPE:
		assert(definition->type->tag == TYPE_MONITOR);
		if (!symtable_insert(state->table, definition)) {
			err_redeclaration(definition->type->monitor.id);
		}
		state->monitor = definition->type;
		symtable_enter_scope(state->table);
		for (Definition* d = definition->type->monitor.definitions; d; d = d->next) {
			sem_definition(state, d);
		}
		symtable_leave_scope(state->table);
		state->monitor = NULL;
		break;
	default:
		UNREACHABLE;
	}
}

static void sem_block(SemanticState* state, Block* block) {
	assert(block->tag == BLOCK);
	for (Block* b = block->next; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DEFINITION:
			for (Definition* d = b->definition; d; d = d->next) {
				sem_definition(state, d);
			}
			continue;
		case BLOCK_STATEMENT:
			sem_statement(state, b->statement);
			continue;
		default:
			UNREACHABLE;
		}
	}
}

static void sem_statement(SemanticState* state, Statement* statement) {
	switch (statement->tag) {
	case STATEMENT_ASSIGNMENT:
		sem_variable(state, &statement->assignment.variable);
		if (statement->assignment.variable->value) { // can't be reassigned
			err_assignment_value(statement);
		}
		if (statement->assignment.variable->tag == VARIABLE_INDEXED &&
			statement->assignment.variable->indexed.array->type->immutable) {
			TODOERR(
				statement->line,
				"can't assign to immutable arrays"
			);
		}
		sem_expression(state, statement->assignment.expression);
		assignment(statement->assignment.variable,
			&statement->assignment.expression);
		break;
	case STATEMENT_FUNCTION_CALL:
		sem_function_call(state, statement->function_call);
		break;
	case STATEMENT_WAIT_FOR_IN:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "wait-for-in");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "wait-for-in");
		}
		sem_expression(state, statement->wait_for_in.condition);
		if (!conditiontype(statement->wait_for_in.condition->type)) {
			err_invalid_condition_type(statement->wait_for_in.condition);
		}
		sem_expression(state, statement->wait_for_in.queue);
		if (statement->wait_for_in.queue->type != __condition_queue) {
			TODOERR(
				statement->line,
				"wait-for-in second expression must be of type ConditionQueue"
			);
		}
		break;
	case STATEMENT_SIGNAL:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "signal");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "signal");
		}
		sem_expression(state, statement->signal);
		if (statement->signal->type != __condition_queue) {
			TODOERR(
				statement->line,
				"signal must receive expression of type ConditionQueue"
			);
		}
		break;
	case STATEMENT_BROADCAST:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "broadcast");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "broadcast");
		}
		sem_expression(state, statement->broadcast);
		if (statement->broadcast->type != __condition_queue) {
			TODOERR(
				statement->line,
				"broadcast must receive expression of type ConditionQueue"
			);
		}
		break;
	case STATEMENT_RETURN:
		// Can't return inside a spawn block
		if (state->spawn.scope) {
			err_return_inside_spawn(statement->line);
		}
		// Can't return an expression inside an initializer
		if (state->initializer) {
			if (statement->return_) {
				err_return_initializer(statement->line);
			} else {
				break;
			}
		}
		// Can't return empty when the function expects a return type
		if (state->return_ != __void && !statement->return_) {	
			err_return_void(statement->line, state->return_);
		}

		if (statement->return_) {
			sem_expression(state, statement->return_);
			typecheck1(state->return_, &statement->return_);
		}
		break;
	case STATEMENT_IF:
		sem_expression(state, statement->if_.expression);
		if (!conditiontype(statement->if_.expression->type)) {
			err_invalid_condition_type(statement->if_.expression);
		}
		symtable_enter_scope(state->table);
		sem_block(state, statement->if_.block);
		symtable_leave_scope(state->table);
		break;
	case STATEMENT_IF_ELSE:
		sem_expression(state, statement->if_else.expression);
		if (!conditiontype(statement->if_else.expression->type)) {
			err_invalid_condition_type(statement->if_else.expression);
		}
		symtable_enter_scope(state->table);
		sem_block(state, statement->if_else.if_block);
		symtable_leave_scope(state->table);
		symtable_enter_scope(state->table);
		sem_block(state, statement->if_else.else_block);
		symtable_leave_scope(state->table);
		break;
	case STATEMENT_WHILE:
		sem_expression(state, statement->while_.expression);
		if (!conditiontype(statement->while_.expression->type)) {
			err_invalid_condition_type(statement->while_.expression);
		}
		symtable_enter_scope(state->table);
		sem_block(state, statement->while_.block);
		symtable_leave_scope(state->table);
		break;
	case STATEMENT_SPAWN: {
		if (state->monitor) {
			TODOERR(statement->line, "can't spawn inside monitors");
		}

		Scope* previous_spawn_scope = state->spawn.scope;
		FunctionCall* previous_spawn_function_call = state->spawn.function_call;

		state->spawn.scope = symtable_enter_scope(state->table);
		state->spawn.function_call = statement->spawn;
		sem_block(state, statement->spawn->function_definition->function.block);
		symtable_leave_scope(state->table);

		state->spawn.scope = previous_spawn_scope;
		state->spawn.function_call = previous_spawn_function_call;
		break;
	}
	case STATEMENT_BLOCK:
		symtable_enter_scope(state->table);
		sem_block(state, statement->block);
		symtable_leave_scope(state->table);
		break;
	default:
		UNREACHABLE;
	}
}

static void sem_variable(SemanticState* state, Variable** variable_pointer) {
	Variable* variable = *variable_pointer;
	assert(!variable->type);

	switch (variable->tag) {
	case VARIABLE_ID: {
		Definition* definition = symtable_find(state->table, variable->id);
		if (!definition) {
			err_variable_unknown(variable->id);
		}
		if (definition->tag != DEFINITION_VARIABLE) {
			err_variable_misuse(variable->id);
		}

		assert(definition->variable.variable->tag == VARIABLE_ID);

		bool from_outside_spawn_scope = false;
		if (state->spawn.scope) {
			if (!symtable_find_in_scope(state->spawn.scope, variable->id)) {
				if (!definition->variable.variable->value) {
					err_spawn_variable(variable->line);
				}
				if (!safetype(definition->variable.variable->type)) {
					err_spawn_unsafe(variable->line);
				}
				from_outside_spawn_scope = true;
			}
		}

		// TODO: This is messy (define lambdas someday)
		if (from_outside_spawn_scope && !variable->global) {
			FunctionCall* call = state->spawn.function_call;
			Definition* function = call->function_definition;
			Type* type = definition->variable.variable->type;

			Variable* parameter_variable = ast_variable_id(
				ast_id(variable->line, variable->id->name)
			);
			parameter_variable->type = type;
			Definition* parameter = ast_definition_variable(
				parameter_variable, NULL
			);
			parameter->variable.variable->value = true;
			parameter->variable.variable->type = type;
			symtable_insert(state->table, parameter);

			Expression* argument = ast_expression_variable(
				definition->variable.variable
			);
			argument->type = type;

			if (call->argument_count == -1) {
				call->arguments = argument;
				call->argument_count = 1;
				function->function.parameters = parameter;
			} else if (call->argument_count > 0) {
				argument->next = call->arguments;
				call->arguments = argument;
				call->argument_count++;
				parameter->next = function->function.parameters;
				function->function.parameters = parameter;
			} else {
				UNREACHABLE;
			}

			free(variable->id);
			free(variable);
			*variable_pointer = parameter->variable.variable;
		} else {
			free(variable->id);
			free(variable);
			*variable_pointer = definition->variable.variable;
		}
		
		break;
	}
	case VARIABLE_INDEXED:
		sem_expression(state, variable->indexed.array);
		sem_expression(state, variable->indexed.index);
		if (variable->indexed.array->type->tag != TYPE_ARRAY) {
			err_variable_array_type(variable);
		}
		if (!indextype(variable->indexed.index->type)) {
			err_variable_array_index_type(variable);
		}
		variable->type = variable->indexed.array->type->array;
		break;
	default:
		UNREACHABLE;
	}
}

static void sem_expression(SemanticState* state, Expression* expression) {
	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		expression->type = __boolean;
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->type = __integer;
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->type = __float;
		break;
	case EXPRESSION_LITERAL_STRING:
		expression->type = __string;
		break;
	case EXPRESSION_VARIABLE:
		sem_variable(state, &expression->variable);
		expression->type = expression->variable->type;
		break;
	case EXPRESSION_FUNCTION_CALL:
		sem_function_call(state, expression->function_call);
		expression->type = expression->function_call->type;
		break;
	case EXPRESSION_UNARY:
		sem_expression(state, expression->unary.expression);
		switch (expression->unary.token) {
		case '-':
			if (!numerictype(expression->unary.expression->type)) {
				err_expression(ERR_EXPRESSION_MINUS, expression);
			}
			expression->type = expression->unary.expression->type;
			break;
		case TK_NOT:
			if (!conditiontype(expression->unary.expression->type)) {
				err_expression(ERR_EXPRESSION_NOT, expression);
			}
			expression->type = __boolean;
			break;
		default:
			UNREACHABLE;
		}
		break;
	case EXPRESSION_BINARY: {
		Expression** lp = &expression->binary.left_expression;
		Expression** rp = &expression->binary.right_expression;
		sem_expression(state, *lp);
		sem_expression(state, *rp);
		switch (expression->binary.token) {
		case TK_OR: case TK_AND:
			if (!conditiontype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT, expression);
			}
			if (!conditiontype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT, expression);
			}
			expression->type = __boolean;
			break;
		case TK_EQUAL: case TK_NEQUAL:
			if (!equatabletype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT_EQUAL, expression);
			}
			if (!equatabletype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT_EQUAL, expression);
			}
			if (!typecheck2(lp, rp)) {
				err_expression(ERR_EXPRESSION_TYPECHECK_EQUAL, expression);
			}
			expression->type = __boolean;
			break;
		case TK_LEQUAL: case TK_GEQUAL: case '<': case '>':
			if (!numerictype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT, expression);
			}
			if (!numerictype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT, expression);
			}
			assert(typecheck2(lp, rp)); // for casting, if necessary
			expression->type = __boolean;
			break;
		case '+': case '-': case '*': case '/':
			if (!numerictype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT, expression);
			}
			if (!numerictype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT, expression);
			}
			expression->type = typecheck2(lp, rp); // for casting, if necessary
			assert(expression->type);
			break;
		default:
			printf("line: %d\n", expression->line);
			UNREACHABLE;
		}
		break;
	}
	case EXPRESSION_CAST:
		UNREACHABLE;
	default:
		UNREACHABLE;
	}
}

// TODO: Doc
// TODO: Currently checking only the first matching method
// TODO: Currently no overloading
static Definition* findmethod(FunctionCall* call) {
	Definition* method_definition = NULL;
	Type* monitor = call->instance->type;

	for (Definition* d = monitor->monitor.definitions; d; d = d->next) {
		if (d->tag == DEFINITION_METHOD) {
			if (d->function.id->name == call->id->name) {
				if (d->function.private) {
					err_function_call_private(call->line, call->id, monitor);
				}
				method_definition = d;
				break;
			}
		}
	}

	if (!method_definition) {
		err_function_call_no_method(call->line, monitor, call->id);
	}

	return method_definition;
}

static void sem_function_call(SemanticState* state, FunctionCall* call) {
	// TODO: calculate argument_count here

	switch (call->tag) {
	case FUNCTION_CALL_BASIC:
		// MEGA TODO: Fix this master gambiarra
		if (!strcmp(call->id->name, "print")) {
			call->argument_count = 0;
			for (Expression* e = call->arguments; e; e = e->next) {
				sem_expression(state, e);
				call->argument_count++;
			}
			call->type = __integer;
			return;
		}

		call->function_definition = symtable_find(state->table, call->id);
		if (!call->function_definition) {
			err_function_call_unknown(call->id);
		}
		if (call->function_definition->tag != DEFINITION_FUNCTION &&
			call->function_definition->tag != DEFINITION_METHOD) {
			// TODO: Can't call constructor from inside a Monitor
			err_function_call_misuse(call->id);
		}

		// Implicit self for method calls inside monitors
		if (call->function_definition->tag == DEFINITION_METHOD) {
			assert(state->monitor);
			PREPEND(Expression, call->arguments,
				ast_expression_variable(astself(call->line)));
		}

		call->type = call->function_definition->function.type;
		free(call->id);
		call->id = NULL;

		// Can't call non-safe functions from inside a monitor
		if (state->monitor && !safetype(call->type)) {
			TODOERR(
				call->line,
				"can't call a function that returns a "
				"non-safe type from inside a monitor"
			);
		}

		break;
	case FUNCTION_CALL_METHOD:
		sem_expression(state, call->instance);
		if (call->instance->type->tag != TYPE_MONITOR) {
			err_function_call_no_monitor(call->line);
		}
		// OBS: Instance type is already linked

		// Finding the function definition in the monitor
		call->function_definition = findmethod(call);
		// Prepending self to arguments
		PREPEND(Expression, call->arguments, call->instance);
		if (call->arguments->next) {
			call->arguments->next->previous = call->instance;
		}

		call->type = call->function_definition->function.type;
		free(call->id);
		call->id = NULL;
		break;
	case FUNCTION_CALL_CONSTRUCTOR: // initializers and arrays
		linktype(state->table, &call->type);

		if (call->type->tag == TYPE_ID) {
			if (call->type != __condition_queue) {
				// TODO: Create test cases for this
				TODOERR(call->line, "type has no known initializer");
			}
			// TODO: Check number of arguments passed (must be zero)
			call->argument_count = 0;
			return;
		}

		// Array constructors must have no arguments or one numeric argument
		if (call->type->tag == TYPE_ARRAY) {
			call->argument_count = 0;
			for (Expression* e = call->arguments; e; e = e->next,
				call->argument_count++);
			if (call->argument_count == 0) { // defaults to 8
				call->argument_count = 1;
				call->arguments = ast_expression_literal_integer(call->line, 8);
				call->arguments->type = __integer;
			} else if (call->argument_count == 1) {
				sem_expression(state, call->arguments);
				typecheck1(__integer, &call->arguments);
			} else {
				err_function_call_array_constructor(
					call->line,
					call->argument_count
				);
			}
			return;
		}

		// Finding the monitor's constructor parameters
		for (Definition* d = call->type->monitor.definitions; d; d = d->next) {
			if (d->tag == DEFINITION_CONSTRUCTOR) {
				// TODO: Currently checking only the first matching constructor
				// TODO: Currently no overloading
				call->function_definition = d;
				break;
			}
		}
		
		if (!call->function_definition) {
			err_function_call_no_constructor(
				call->line,
				call->type->monitor.id
			);
		}
		break;
	default:
		UNREACHABLE;
	}

	assert(call->function_definition);
	Definition* parameter = call->function_definition->function.parameters;
	Expression* argument = call->arguments;
	Expression** pointer = &call->arguments;
	call->argument_count = 0;

	// Skips comparing between the first parameter and argument of a method
	if (call->tag == FUNCTION_CALL_METHOD) {
		assert(parameter && argument);
		call->argument_count++;
		parameter = parameter->next;
		argument = (*pointer)->next;
		pointer = &argument;
	}

	// Comparing arguments with parameters
	while (parameter || argument) {
		if (parameter && !argument) {
			err_function_call_few_args(call->line);
		}
		if (!parameter && argument) {
			err_function_call_excess_args(call->line);
		}

		call->argument_count++;

		sem_expression(state, argument);
		typecheck1(parameter->variable.variable->type, pointer);
		parameter = parameter->next;
		argument = (*pointer)->next;
		pointer = &argument;
	}
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO
static bool typeequals(Type* type1, Type* type2) {
	if (type1->immutable != type2->immutable) {
		return false;
	}
	if (type1 == type2) {
		return true;
	}
	if (type1->tag != TYPE_ARRAY || type2->tag != TYPE_ARRAY) {
		return false;
	}

	return typeequals(type1->array, type2->array);
}

// static void freetype(Type* type) {
// 	switch (type->tag) {
// 	case TYPE_ID:
// 		freetypeid(type);
// 		break;
// 	case TYPE_ARRAY:
// 		freetypearray(type);
// 		break;
// 	default:
// 		UNREACHABLE;
// 	}
// }

static void freetypeid(Type* type) {
	assert(type->tag == TYPE_ID);
	if (type->primitive) {
		return;
	}
	free(type->id);
	free(type);
}

// static void freetypearray(Type* type) {
// 	if (type->tag == TYPE_ID) {
// 		freetypeid(type);
// 		return;
// 	}

// 	assert(type->tag == TYPE_ARRAY);
// 	freetypearray(type->array);
// 	free(type);
// }

/*
 * Replaces an id-type for its declaration equivalent using the symbol table.
 * Deals with errors internally.
 */
static void linktype(SymbolTable* table, Type** pointer) {
	assert(pointer && *pointer);

	switch ((*pointer)->tag) {
	case TYPE_VOID:
		// Not necessary to look for in the symbol table because
		// TypeVoid instance is not provided by the user as an id
		break;
	case TYPE_ID: {
		Definition* definition = symtable_find(table, (*pointer)->id);
		if (!definition) {
			err_unkown_type_name((*pointer)->id);
		}
		assert(definition->tag == DEFINITION_TYPE);
		freetypeid(*pointer);
		(*pointer) = definition->type;
		break;
	}
	case TYPE_ARRAY:
		for (; (*pointer)->tag == TYPE_ARRAY; pointer = &(*pointer)->array);
		linktype(table, pointer);
		break;
	default:
		UNREACHABLE;
	}
}

/*
 * Checks for type equivalence between variable and expression.
 * Works for definitions and simple assignments.
 * Deals with errors internally.
 */
static void assignment(Variable* variable, Expression** expression) {
	if (variable->type) {
		typecheck1(variable->type, expression);
	} else { // when defining with implicit type
		variable->type = (*expression)->type;
	}
}

/* 
 * Checks for type equivalence of one expression.
 * Performes casts if necessary.
 * Deals with errors internally.
 */
static void typecheck1(Type* type, Expression** expression) {
	// Checks equality
	if (typeequals(type, (*expression)->type)) {
		return;
	}

	// Needs both to be numeric
	if (!(numerictype(type) && numerictype((*expression)->type))) {
		err_type((*expression)->line, type, (*expression)->type);
	}

	// Performs casts
	*expression = ast_expression_cast(*expression, type);
}

/* 
 * Checks for type equivalence of two expressions.
 * Performes casts if necessary.
 * Returns NULL if types are incompatible.
 */
static Type* typecheck2(Expression** l, Expression** r) {
	Type* t1 = (*l)->type;
	Type* t2 = (*r)->type;

	// Checks equality
	if (typeequals(t1, t2)) {
		return t1;
	}

	// Needs both to be numeric
	if (!(numerictype(t1) && numerictype(t2))) {
		return NULL;
	}

	// Performs casts
	if (t1 == __float) {
		return (*r = ast_expression_cast(*r, __float))->type;
	}
	if (t2 == __float) {
		return (*l = ast_expression_cast(*l, __float))->type;
	}

	UNREACHABLE;
}

static bool indextype(Type* type) {
	return type == __integer; // TODO: Float also
}

static bool numerictype(Type* type) {
	return type == __integer || type == __float;
}

static bool conditiontype(Type* type) {
	return type == __boolean;
}

static bool equatabletype(Type* type) { // TODO: Strings?
	return type == __boolean || type == __integer || type == __float;
}

static bool safetype(Type *type) {
	switch (type->tag) {
		case TYPE_VOID:
		case TYPE_ID:
			return type->immutable;
		case TYPE_ARRAY:
			return type->immutable && safetype(type->array);
		case TYPE_MONITOR:
			return true;
	}
	UNREACHABLE;
}

// ==================================================
//
//	Errors
//
// ==================================================

// Returns the string corresponding to the type
static const char* typestring(Type* type) {
	assert(type);
	char* str;

	switch (type->tag) {
	case TYPE_VOID:
		str = "Void";
		break;
	case TYPE_ID:
		str = (char*) type->id->name;
		break;
	case TYPE_ARRAY: {
		unsigned int counter = 0;
		Type* t = type;
		for (; t->tag == TYPE_ARRAY; t = t->array, counter++);
		const char* id = typestring(t);
		size_t len = 2 * counter + strlen(id);
		MALLOC_ARRAY(str, char, len + 1);
		strcpy(str + counter, id);
		for (int i = 0; i < counter; i++) {
			str[i] = '[';
			str[len - i - 1] = ']';
		}
		str[len] = '\0';
		break;
	}
	case TYPE_MONITOR:
		str = (char*) type->monitor.id->name;
		break;
	default:
		UNREACHABLE;
	}

	if (!type->primitive && type->immutable) {
		const char* immutable = "Immutable ";
		size_t lenImmut = strlen(immutable), lenStr = strlen(str);
		size_t len = lenImmut + lenStr;
		char *str2;
		MALLOC_ARRAY(str2, char, len + 1);
		str2[len] = '\0';
		strcpy(str2, immutable);
		strcpy(&str2[lenImmut], str);
		str = str2;
	}

	return str;
}

static const char* tokenstring(Token token) {
	switch (token) {
    case TK_OR:		return "or";
    case TK_AND:	return "and";
    case TK_EQUAL:	return "==";
    case TK_NEQUAL:	return "!=";
    case TK_LEQUAL:	return "<=";
    case TK_GEQUAL:	return ">=";
    case '<':		return "<";
    case '>':		return ">";
    case '+':		return "+";
    case '-':		return "-";
    case '*':		return "*";
    case '/':		return "/";
    default:
    	UNREACHABLE;
	}
}

// TODO: These errs with variadic functions

// Creates a formatted error with one dynamic string
static const char* err1(const char* format, const char* name) {
	char* err;
	size_t len = strlen(format) - 2 + strlen(name); // -2 for '%s'
	MALLOC_ARRAY(err, char, len + 1);
	sprintf(err, format, name);
	err[len] = '\0';
	return err;
}

// Creates a formatted error with two dynamic strings
static const char* err2(const char* format, const char* s1, const char* s2) {
	char* err;
	size_t len = strlen(format) - 4 + strlen(s1) + strlen(s2); // -4 for '%s'
	MALLOC_ARRAY(err, char, len + 1);
	sprintf(err, format, s1, s2);
	err[len] = '\0';
	return err;
}

// Creates a formatted error with three dynamic strings
static const char* err3(const char* format, const char* s1, const char* s2,
	const char* s3) {

	char* err;
	// -6 for '%s'
	size_t len = strlen(format) - 6 + strlen(s1) + strlen(s2) + strlen(s3);
	MALLOC_ARRAY(err, char, len + 1);
	sprintf(err, format, s1, s2, s3);
	err[len] = '\0';
	return err;
}

static void err_redeclaration(Id* id) {
	sem_error(id->line, err1("redeclaration of name '%s'", id->name));
}

static void err_unkown_type_name(Id* id) {
	sem_error(id->line, err1("unkown type name '%s'", id->name));
}

static void err_invalid_condition_type(Expression* expression) {
	const char* err = typestring(expression->type);
	err = err1("invalid type '%s' for condition (expecting Boolean)", err);
	sem_error(expression->line, err);
}

static void err_type(Line line, Type* type1, Type* type2) {
	const char* t1 = typestring(type1);
	const char* t2 = typestring(type2);
	const char* err = err2("type error (expecting '%s', got '%s')", t1, t2);
	sem_error(line, err);
}

static void err_assignment_value(Statement* statement) {
	const char* err =
		err1("can't assign to '%s' since it was declared as a value",
			statement->assignment.variable->id->name);
	sem_error(statement->line, err);
}

static void err_return_inside_spawn(Line line) {
	sem_error(line, "can't return inside a spawn block");
}

static void err_return_initializer(Line line) {
	sem_error(line, "can't return expression inside initializer");
}

static void err_return_void(Line line, Type* type) {
	const char* err = typestring(type);
	err = err1("return can't be empty, "
		"must return an expression of type '%s'", err);
	sem_error(line, err);
}

static void err_variable_unknown(Id* id) {
	const char* err = err1("unknown variable '%s' beeing used", id->name);
	sem_error(id->line, err);
}

static void err_variable_misuse(Id* id) {
	const char* err = err1("'%s' is not a variable", id->name);
	sem_error(id->line, err);
}

static void err_variable_array_type(Variable* variable) {
	const char* err = typestring(variable->indexed.array->type);
	err = err1("type '%s' can't be used as an array", err);
	sem_error(variable->line, err);
}

static void err_variable_array_index_type(Variable* variable) {
	const char* err = typestring(variable->indexed.index->type);
	err = err1("type '%s' can't be used as an array index", err);
	sem_error(variable->line, err);
}

static void err_function_call_unknown(Id* id) {
	const char* err = err1("unknown function '%s' beeing called", id->name);
	sem_error(id->line, err);
}

static void err_function_call_misuse(Id* id) {
	const char* err = err1("'%s' is not a function", id->name);
	sem_error(id->line, err);
}

static void err_function_call_array_constructor(Line line, unsigned int n) {
	char num[15];
	sprintf(num, "%d", n);
	const char* err = err1("an array constructor must have "
		"zero or one arguments, not %s", num);
	sem_error(line, err);
}

static void err_function_call_no_constructor(Line line, Id* id) {
	const char* err = err1("monitor '%s' has no defined initializer", id->name);
	sem_error(line, err);
}

static void err_function_call_few_args(Line line) {
	sem_error(line, "function call has too few arguments");
}

static void err_function_call_excess_args(Line line) {
	sem_error(line, "function call has too many arguments");
}

static void err_function_call_no_monitor(Line line) {
	sem_error(line, "trying to call a method on a type that is not a monitor");
}

static void err_function_call_private(Line line, Id* id, Type* type) {
	const char* err = err2("method '%s' from monitor '%s' is private",
		id->name, typestring(type));
	sem_error(line, err);
}

static void err_function_call_no_method(Line line, Type* type, Id* id) {
	const char* err = err2("monitor '%s' has no defined method '%s'",
		typestring(type), id->name);
	sem_error(line, err);
}

static void err_monitor_statements(Line line, const char* statement) {
	const char* err = err1("invalid use of '%s' statement "
		"outside a monitor's body", statement);
	sem_error(line, err);
}

static void err_monitor_statements_constructor(Line line, const char* stmt) {
	const char* err = err1("invalid use of '%s' statement "
		"inside a monitor's initializer", stmt);
	sem_error(line, err);
}

static void err_monitor_function_type(Line line) {
	sem_error(line, "a monitor's function can only return "
		"immutable or monitor types");
}

static void err_spawn_variable(Line line) {
	sem_error(line, "can't access a variable inside a spawn block "
		"(only values)");
}

static void err_spawn_unsafe(Line line) {
	sem_error(line, "can't access a value of unsafe type "
		"inside a spawn block (only immutables and monitors)");
}

// ==================================================
//
//	TODO: Experimental
//
// ==================================================

static const char* errors[] = {
	"invalid type '%s' for unary minus (expecting numeric type)",
	"invalid type '%s' for unary not (expecting Boolean)",

	"invalid type '%s' for the left side of the '%s' expression",
	"invalid type '%s' for the right side of the '%s' expression",

	"invalid type for left side of the '==' ('%s' is not an equatable type)",
	"invalid type for right side of the '==' ('%s' is not an equatable type)",
	"incompatible types '%s' and '%s' for '%s' expression"
};

static void err_expression(ErrorType type, Expression* e) {
	Line line = e->line;
	const char* err = NULL;

	switch (type) {
	case ERR_EXPRESSION_MINUS:
	case ERR_EXPRESSION_NOT:
		err = err1(errors[type], typestring(e->unary.expression->type));
		break;
	case ERR_EXPRESSION_LEFT:
		err = err2(errors[type], typestring(e->binary.left_expression->type),
			tokenstring(e->binary.token));
		break;
	case ERR_EXPRESSION_RIGHT:
		err = err2(errors[type], typestring(e->binary.right_expression->type),
			tokenstring(e->binary.token));
		break;
	case ERR_EXPRESSION_LEFT_EQUAL:
		err = err1(errors[type], typestring(e->binary.left_expression->type));
		break;
	case ERR_EXPRESSION_RIGHT_EQUAL:
		err = err1(errors[type], typestring(e->binary.right_expression->type));
		break;
	case ERR_EXPRESSION_TYPECHECK_EQUAL:
		err = err3(errors[type], typestring(e->binary.left_expression->type),
			typestring(e->binary.right_expression->type),
			tokenstring(e->binary.token));
		break;
	default:
		UNREACHABLE;
	}

	sem_error(line, err);
}
