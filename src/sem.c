/*
 * TODO
 *
 *	- Immutable Monitor1 is wrong
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

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL)

// TODO
#define TODOERR(line, err) \
	printf("line %d:\n\tsemantic error: %s\n", line, err); exit(1); \

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

	// <true> if currently analysing a spawn statement block, <false> otherwise
	bool spawn;
} SemanticState;

// Primitive types
static Type* void_;
static Type* boolean_;
static Type* integer_;
static Type* float_;
static Type* string_;

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
	void_ = ast_type_void();
	boolean_ = ast_type_boolean();
	integer_ = ast_type_integer();
	float_ = ast_type_float();
	string_ = ast_type_string();
	Definition* definition_boolean = ast_definition_type(boolean_);
	Definition* definition_integer = ast_definition_type(integer_);
	Definition* definition_float = ast_definition_type(float_);
	Definition* definition_string = ast_definition_type(string_);

	SemanticState state = {
		/* symbol table			*/ symtable_new(),
		/* current monitor		*/ NULL,
		/* return type			*/ NULL,
		/* inside initializer	*/ false,
		/* inside spawn			*/ false
	};

	// Analysis
	symtable_enter_scope(state.table);
	symtable_insert(state.table, definition_boolean);
	symtable_insert(state.table, definition_integer);
	symtable_insert(state.table, definition_float);
	symtable_insert(state.table, definition_string);
	for (Definition* d = ast->definitions; d; d = d->next) {
		sem_definition(&state, d);
	}
	symtable_leave_scope(state.table);

	// Teardown
	symtable_free(state.table);
	free(definition_boolean);
	free(definition_integer);
	free(definition_float);
	free(definition_string);

	assert(!state.monitor);
	assert(!state.return_);
	assert(!state.initializer);
	assert(!state.spawn);
}

// ==================================================
//
//	Implementation
//
// ==================================================

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
	case DEFINITION_CONSTRUCTOR:
		state->initializer = true;
		/* fallthrough */
	case DEFINITION_FUNCTION:
		assert(!definition->function.private);
		/* fallthrough */
	case DEFINITION_METHOD:
		if (definition->function.id) { // Functions and methods
			if (!symtable_insert(state->table, definition)) {
				err_redeclaration(definition->function.id);
			}
			if (state->monitor && !safetype(definition->function.type)) {
				err_monitor_function_type(definition->function.id->line);
			}
			linktype(state->table, &definition->function.type);
		} else { // Constructors
			assert(state->monitor);
			definition->function.id = state->monitor->monitor.id;
			definition->function.type = state->monitor;
		}

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
	case DEFINITION_TYPE:
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
			sem_definition(state, b->definition);
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
		if (statement->assignment.variable->value) { // Can't be reassigned
			err_assignment_value(statement);
		}
		sem_expression(state, statement->assignment.expression);
		assignment(statement->assignment.variable,
			&statement->assignment.expression);
		break;
	case STATEMENT_FUNCTION_CALL:
		sem_function_call(state, statement->function_call);
		break;
	case STATEMENT_WHILE_WAIT:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "while-wait");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "while-wait");
		}
		sem_expression(state, statement->while_wait.expression);
		if (!conditiontype(statement->while_wait.expression->type)) {
			err_invalid_condition_type(statement->while_wait.expression);
		}
		// TODO: Special semantics for condition variables
		sem_variable(state, &statement->while_wait.variable);
		break;
	case STATEMENT_SIGNAL:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "signal");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "signal");
		}
		// TODO: Special semantics for condition variables
		sem_variable(state, &statement->signal);
		break;
	case STATEMENT_BROADCAST:
		if (!state->monitor) {
			err_monitor_statements(statement->line, "broadcast");
		}
		if (state->initializer) {
			err_monitor_statements_constructor(statement->line, "broadcast");
		}
		// TODO: Special semantics for condition variables
		sem_variable(state, &statement->broadcast);
		break;
	case STATEMENT_RETURN:
		// Can't return inside a spawn block
		if (state->spawn) {
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
		if (state->return_ != void_ && !statement->return_) {	
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
	case STATEMENT_SPAWN:
		// TODO: Should not be able to spawn inside a monitor and initializer
		symtable_enter_scope(state->table);
		state->spawn = true;
		sem_block(state, statement->spawn);
		state->spawn = false;
		symtable_leave_scope(state->table);
		break;
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

	switch (variable->tag) {
	case VARIABLE_ID: {
		int n = 0;
		Definition* definition = symtable_find(state->table, variable->id, &n);
		if (!definition) {
			err_variable_unknown(variable->id);
		} else if (definition->tag != DEFINITION_VARIABLE) {
			err_variable_misuse(variable->id);
		}

		assert(definition->variable.variable->tag == VARIABLE_ID);
		assert(!variable->type);
		Line line = variable->line;
		free(variable->id);
		free(variable);
		*variable_pointer = variable = definition->variable.variable;

		// If accessing variables from outside a spawn block
		if (state->spawn && n != 1) {
			if (!variable->value) {
				err_spawn_variable(line);
			}
			if (!safetype(variable->type)) {
				err_spawn_unsafe(line);
			}
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
		expression->type = boolean_;
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->type = integer_;
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->type = float_;
		break;
	case EXPRESSION_LITERAL_STRING:
		expression->type = string_;
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
			expression->type = boolean_;
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
			expression->type = boolean_;
			break;
		case TK_EQUAL:
			if (!equatabletype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT_EQUAL, expression);
			}
			if (!equatabletype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT_EQUAL, expression);
			}
			if (!typecheck2(lp, rp)) {
				err_expression(ERR_EXPRESSION_TYPECHECK_EQUAL, expression);
			}
			expression->type = boolean_;
			break;
		case TK_LEQUAL: case TK_GEQUAL: case '<': case '>':
			if (!numerictype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT, expression);
			}
			if (!numerictype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT, expression);
			}
			assert(typecheck2(lp, rp)); // for casting, if necessary
			expression->type = boolean_;
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

static void sem_function_call(SemanticState* state, FunctionCall* call) {
	switch (call->tag) {
	case FUNCTION_CALL_BASIC:
		// MEGA TODO: Fix this master gambiarra
		if (!strcmp(call->id->name, "print")) {
			for (Expression* e = call->arguments; e; e = e->next) {
				sem_expression(state, e);
			}
			call->type = integer_;
			return;
		}

		call->function_definition = symtable_find(state->table, call->id, NULL);
		if (!call->function_definition) {
			err_function_call_unknown(call->id);
		} else if (call->function_definition->tag != DEFINITION_FUNCTION &&
			call->function_definition->tag != DEFINITION_METHOD) {
			// TODO: Can't call constructor from inside a Monitor
			err_function_call_misuse(call->id);
		}

		call->type = call->function_definition->function.type;
		free(call->id);
		call->id = NULL;
		break;
	case FUNCTION_CALL_METHOD:
		sem_expression(state, call->instance);
		if (call->instance->type->tag != TYPE_MONITOR) {
			err_function_call_no_monitor(call->line);
		}
		// Instance type is already linked
		for (Definition* d = call->instance->type->monitor.definitions; d;
			d = d->next) {

			if (d->tag == DEFINITION_METHOD) {
				if (d->function.id->name == call->id->name) {
					// TODO: Currently checking only the first matching method
					// TODO: Currently no overloading
					if (d->function.private) {
						err_function_call_private(call->line, call->id,
							call->instance->type);
					}
					call->function_definition = d;
				}
				break;
			}
		}
		
		if (!call->function_definition) {
			err_function_call_no_method(call->line, call->instance->type, call->id);
		}
		call->type = call->function_definition->function.type;
		free(call->id);
		call->id = NULL;
		break;
	case FUNCTION_CALL_CONSTRUCTOR: // initializers and arrays
		linktype(state->table, &call->type);

		// Array constructors must have no arguments or one numeric argument
		if (call->type->tag == TYPE_ARRAY) {
			call->arguments_count = 0;
			for (Expression* e = call->arguments; e; e = e->next,
				call->arguments_count++);
			if (call->arguments_count == 0) { // defaults to 8
				call->arguments = ast_expression_literal_integer(call->line, 8);
				call->arguments->type = integer_;
			} else if (call->arguments_count == 1) {
				sem_expression(state, call->arguments);
				typecheck1(integer_, &call->arguments);
			} else {
				err_function_call_array_constructor(
					call->line,
					call->arguments_count
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

	// Comparing arguments with parameters
	while (parameter || argument) {
		if (parameter && !argument) {
			err_function_call_few_args(call->line);
		}
		if (!parameter && argument) {
			err_function_call_excess_args(call->line);
		}

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
		Definition* definition = symtable_find(table, (*pointer)->id, NULL);
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
	if (t1 == float_) {
		return (*r = ast_expression_cast(*r, float_))->type;
	}
	if (t2 == float_) {
		return (*l = ast_expression_cast(*l, float_))->type;
	}

	UNREACHABLE;
}

static bool indextype(Type* type) {
	return type == integer_; // TODO: Float also
}

static bool numerictype(Type* type) {
	return type == integer_ || type == float_;
}

static bool conditiontype(Type* type) {
	return type == boolean_;
}

static bool equatabletype(Type* type) { // TODO: Strings?
	return type == boolean_ || type == integer_ || type == float_;
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

// TODO: This errs with variadic functions

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
