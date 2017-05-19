/*
 * TODO:
 *
 *	- initializer(): Semântica? Pode ter mais de um?
 *		Pode ter mais de um com tipos / quantidade
 *		diferentes de parâmetros?
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

// TODO: Remove and deal the with errors
static void todoerr(const char* err) {
	printf("%s\n", err);
	exit(1);
}

/*
 * Symbol table for declarations and types (TODO)
 */
static SymbolTable *ltable, *utable;

/*
 * A monitor type instance if currently analysing a monitor's
 * body (NULL otherwise).
 */
static Type* monitor = NULL;

// Primitive types
static Type* boolean_;
static Type* integer_;
static Type* float_;
static Type* string_;

// Functions that analyse the abstract syntax tree recursively
static void sem_body(Body*);
static void sem_declaration(Declaration*);
static void sem_definition(Definition*);
static void sem_block(Block*, Type*); // return type
static void sem_statement(Statement*, Type*); // return type
static void sem_variable(Variable*);
static void sem_expression(Expression*);
static void sem_function_call(FunctionCall*);

// Auxiliary functions that deal with type analysis
static void enterscope(void);
static void leavescope(void);
static void linktype(Type**);
static void assignment(Variable*, Expression**);
static void typecheck(Type*, Expression**);
static Type* typecast(Expression**, Expression**);
static bool indextype(Type*);
static bool numerictype(Type*);
static bool conditiontype(Type*);
static bool equatabletype(Type*);

// Auxiliary functions that deal with errors
static void err_redeclaration(Id*);
static void err_unkown_type_name(Id*);
static void err_invalid_condition_type(Expression*);

/*
 * TODO
 */
void sem_analyse(Program* program) {
	// Setup
	ltable = symtable_new();
	utable = symtable_new();
	boolean_ = ast_type_boolean();
	integer_ = ast_type_integer();
	float_ = ast_type_float();
	string_ = ast_type_string();

	// Analysis
	enterscope();
	symtable_insert_type(utable, boolean_);
	symtable_insert_type(utable, integer_);
	symtable_insert_type(utable, float_);
	symtable_insert_type(utable, string_);
	sem_body(program->body);
	leavescope();

	// Teardown
	symtable_free(ltable);
	symtable_free(utable);
}

// ==================================================
//
//	Recursive functions
//
// ==================================================

static void sem_body(Body* body) {
	if (body->tag == BODY) {
		if (body->next) {
			sem_body(body->next);
		}
		return;
	}

	for (Body* b = body; b; b = b->next) {
		switch (b->tag) {
		case BODY_DECLARATION:
			sem_declaration(b->declaration);
			break;
		case BODY_DEFINITION:
			sem_definition(b->definition);
			break;
		default:
			assert(b->tag != BODY);
		}
	}
}

static void sem_declaration(Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		if (!symtable_insert_declaration(ltable, declaration)) {
			err_redeclaration(declaration->variable->id);
		}
		if (declaration->variable->type) { // inferred declarations have no type
			linktype(&declaration->variable->type);
		}
		break;
	case DECLARATION_FUNCTION:
		if (declaration->function.id) { // constructors don't have an id
			if (!symtable_insert_declaration(ltable, declaration)) {
				err_redeclaration(declaration->function.id);
			}
			if (declaration->function.type) { // some functions return nothing
				linktype(&declaration->function.type);
			}
		} else { // constructors need to be given a type
			assert(monitor);
			declaration->function.type = monitor;
		}
		enterscope(); // should leave in sem_definition
		for (Declaration* p = declaration->function.parameters; p;
			sem_declaration(p), p = p->next);
		break;
	}
}

static void sem_definition(Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE:
		sem_expression(definition->variable.expression);
		sem_declaration(definition->variable.declaration);
		assignment(definition->variable.declaration->variable,
			&definition->variable.expression);
		break;
	case DEFINITION_FUNCTION:
	case DEFINITION_CONSTRUCTOR:
		sem_declaration(definition->function.declaration);
		sem_block(definition->function.block,
			definition->function.declaration->function.type);
		leavescope(); // entered in sem_declaration
		break;
	case DEFINITION_METHOD:
		sem_definition(definition->method.function);
		break;
	case DEFINITION_TYPE:
		if (!symtable_insert_type(utable, definition->type)) {
			err_redeclaration(definition->type->monitor.id);
		}
		monitor = definition->type;
		enterscope();
		sem_body(definition->type->monitor.body);
		leavescope();
		monitor = NULL;
		break;
	}
}

static void sem_block(Block* block, Type* return_type) {
	if (block->tag == BLOCK) {
		if (block->next) {
			sem_block(block->next, return_type);
		}
		return;
	}

	for (Block* b = block; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			sem_declaration(b->declaration);
			break;
		case BLOCK_DEFINITION:
			sem_definition(b->definition);
			break;
		case BLOCK_STATEMENT:
			sem_statement(b->statement, return_type);
			break;
		default:
			assert(b->tag != BLOCK);
		}
	}
}

// TODO: Line for literal expressions
static void sem_statement(Statement* statement, Type* return_type) {
	switch (statement->tag) {
	case STATEMENT_ASSIGNMENT:
		sem_variable(statement->assignment.variable);
		sem_expression(statement->assignment.expression);
		assignment(statement->assignment.variable,
			&statement->assignment.expression);
		break;
	case STATEMENT_FUNCTION_CALL:
		sem_function_call(statement->function_call);
		break;
	case STATEMENT_WHILE_WAIT:
		sem_expression(statement->while_wait.expression);
		if (!conditiontype(statement->while_wait.expression->type)) {
			err_invalid_condition_type(statement->while_wait.expression);
		}
		// TODO: Special semantics for condition variables?
		sem_variable(statement->while_wait.variable);
		break;
	case STATEMENT_SIGNAL:
		// TODO: Special semantics for condition variables?
		sem_variable(statement->signal);
		break;
	case STATEMENT_BROADCAST:
		// TODO: Special semantics for condition variables?
		sem_variable(statement->broadcast);
		break;
	case STATEMENT_RETURN:
		if (statement->return_) {
			sem_expression(statement->return_);
			typecheck(return_type, &statement->return_);
		} else if (return_type) {
			todoerr("invalid type for return");
		}
		break;
	case STATEMENT_IF:
		sem_expression(statement->if_.expression);
		if (!conditiontype(statement->if_.expression->type)) {
			err_invalid_condition_type(statement->if_.expression);
		}
		enterscope();
		sem_block(statement->if_.block, return_type);
		leavescope();
		break;
	case STATEMENT_IF_ELSE:
		sem_expression(statement->if_else.expression);
		if (!conditiontype(statement->if_else.expression->type)) {
			err_invalid_condition_type(statement->if_else.expression);
		}
		enterscope();
		sem_block(statement->if_else.if_block, return_type);
		leavescope();
		enterscope();
		sem_block(statement->if_else.else_block, return_type);
		leavescope();
		break;
	case STATEMENT_WHILE:
		sem_expression(statement->while_.expression);
		if (!conditiontype(statement->while_.expression->type)) {
			err_invalid_condition_type(statement->while_.expression);
		}
		enterscope();
		sem_block(statement->while_.block, return_type);
		leavescope();
		break;
	case STATEMENT_SPAWN:
		enterscope();
		// TODO: Special semantics for spawn blocks
		sem_block(statement->spawn, return_type);
		leavescope();
		break;
	case STATEMENT_BLOCK:
		enterscope();
		sem_block(statement->block, return_type);
		leavescope();
		break;
	}
}

static void sem_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID: {
		Declaration* declaration =
			declaration = symtable_find_declaration(ltable, variable->id);
		if (!declaration) {
			todoerr("variable not defined");
		} else if (declaration->tag != DECLARATION_VARIABLE) {
			todoerr("not a variable");
		}
		free(variable->id);
		variable->id = declaration->variable->id;
		variable->type = declaration->variable->type;
		break;
	}
	case VARIABLE_INDEXED:
		sem_expression(variable->indexed.array);
		sem_expression(variable->indexed.index);
		if (variable->indexed.array->type->tag != TYPE_ARRAY) {
			todoerr("not array type");
		}
		if (!indextype(variable->indexed.index->type)) {
			todoerr("invalid index type for array");
		}
		variable->type = variable->indexed.array->type->array;
		break;
	}
}

static void sem_expression(Expression* expression) {
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
		sem_variable(expression->variable);
		expression->type = expression->variable->type;
		break;
	case EXPRESSION_FUNCTION_CALL:
		sem_function_call(expression->function_call);
		expression->type = expression->function_call->type;
		break;
	case EXPRESSION_UNARY:
		sem_expression(expression->unary.expression);
		switch (expression->unary.token) {
		case '-':
			if (!numerictype(expression->unary.expression->type)) {
				todoerr("invalid type for unary minus");
			}
			expression->type = expression->unary.expression->type;
			break;
		case TK_NOT:
			if (!conditiontype(expression->unary.expression->type)) {
				todoerr("invalid type for unary not");
			}
			expression->type = boolean_;
			break;
		}
		break;
	case EXPRESSION_BINARY: {
		Expression** lp = &expression->binary.left_expression;
		Expression** rp = &expression->binary.right_expression;
		sem_expression(*lp);
		sem_expression(*rp);
		switch (expression->binary.token) {
		case TK_OR: case TK_AND:
			if (!conditiontype((*lp)->type) || !conditiontype((*rp)->type)) {
				todoerr("invalid type for the left side of the or/and expression");
			}
			expression->type = boolean_;
			break;
		case TK_EQUAL:
			if (!equatabletype((*lp)->type)) {
				todoerr("invalid type for left comparison expression");
			}
			if (!equatabletype((*rp)->type)) {
				todoerr("invalid type for right comparison expression");
			}
			typecast(lp, rp); // TODO: Won't work
			expression->type = boolean_;
			break;
		case TK_LEQUAL: case TK_GEQUAL: case '<': case '>':
			if (!numerictype((*lp)->type)) {
				todoerr("invalid type for left comparison expression");
			}
			if (!numerictype((*rp)->type)) {
				todoerr("invalid type for right comparison expression");
			}
			typecast(lp, rp); // TODO: Won't work
			expression->type = boolean_;
			break;
		case '+': case '-': case '*': case '/':
			if (!numerictype((*lp)->type)) {
				todoerr("invalid type for left arith expression");
			}
			if (!numerictype((*rp)->type)) {
				todoerr("invalid type for right arith expression");
			}
			expression->type = typecast(lp, rp); // TODO: Won't work
			break;
		}
		break;
	}
	case EXPRESSION_CAST:
		assert(expression->tag != EXPRESSION_CAST);
		break;
	}
}

static void sem_function_call(FunctionCall* function_call) {
	Declaration* declaration;

	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC:
		declaration = symtable_find_declaration(ltable, function_call->basic);
		if (!declaration) {
			todoerr("function not defined");
		} else if (declaration->tag != DECLARATION_FUNCTION) {
			todoerr("not a function");
		}
		free(function_call->basic);
		function_call->type = declaration->function.type;
		function_call->basic = declaration->function.id;
		break;
	case FUNCTION_CALL_METHOD:
		// sem_expression(function_call->method.object);
		// Body* body = function_call->method.object->type->monitor->monitor.body;

		// check if function_call->method.name is inside the class
		// check if arguments match parameters

		break;
	case FUNCTION_CALL_CONSTRUCTOR:
		linktype(&function_call->constructor);
		function_call->type = function_call->constructor;
		break;
	}

	// TODO
	// Checking the arguments with the parameters
	for (Expression* e = function_call->arguments; e; e = e->next) {
		// TODO
		sem_expression(e);
	}
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

static void freetypeid(Type* type) {
	assert(type->tag == TYPE_ID);
	if (type->primitive) {
		return;
	}
	free(type->id);
	free(type);
}

// TODO
static void enterscope(void) {
	symtable_enter_scope(ltable);
	symtable_enter_scope(utable);
}

// TODO
static void leavescope(void) {
	symtable_leave_scope(ltable);
	symtable_leave_scope(utable);
}

/*
 * Replaces an id-type for its declaration equivalent using the symbol table.
 * Deals with errors internally.
 */
static void linktype(Type** pointer) {
	assert(pointer && *pointer);

	switch ((*pointer)->tag) {
	case TYPE_ID: {
		Type* type = *pointer;
		if (!(*pointer = symtable_find_type(utable, type->id))) {
			err_unkown_type_name(type->id);
		}
		freetypeid(type);
		break;
	}
	case TYPE_ARRAY:
		for (; (*pointer)->tag == TYPE_ARRAY; pointer = &(*pointer)->array);
		linktype(pointer);
		break;
	default:
		assert(NULL); // unreachable
	}
}

/*
 * Checks for type equivalence between variable and expression.
 * Works for definitions and simple assignments.
 * Deals with errors internally.
 */
static void assignment(Variable* variable, Expression** expression) {
	if (!(*expression)->type) { // when defining with implicit type
		todoerr("can't assign variable to type void");
	}
	if (variable->type) {
		typecheck(variable->type, expression);
	} else { // when defining with implicit type
		variable->type = (*expression)->type;
	}
}

/* 
 * Checks for type equivalence (performing casts if necessary).
 * Deals with errors internally.
 */
static void typecheck(Type* type, Expression** expression) {
	bool numeric1 = numerictype(type);
	bool numeric2 = numerictype((*expression)->type);

	if ((numeric1 && !numeric2) || (!numeric1 && numeric2)) {
		todoerr("mismatching types");
	}

	// Both types are not numeric and are different
	if (!numeric1 && type != (*expression)->type) {
		todoerr("mismatching types");
	}

	// Casting for when both are numeric types
	if (type != (*expression)->type) {
		*expression = ast_expression_cast(*expression, type);
	}
}

/*
 * Casts one of the expressions if they have mismatching types.
 * Casts always go from integer to float.
 * The expressions must be of number type.
 */
static Type* typecast(Expression** le, Expression** re) {
	assert(numerictype((*le)->type) && numerictype((*re)->type));

	if ((*le)->type == (*re)->type) {
		return (*le)->type;
	}

	if ((*le)->type == float_) {
		return (*re = ast_expression_cast(*re, float_))->type;
	}
	if ((*re)->type == float_) {
		return (*le = ast_expression_cast(*le, float_))->type;
	}

	return assert(NULL), NULL; // unreachable
}

static bool indextype(Type* type) {
	return type == integer_;
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

// ==================================================
//
//	Errors
//
// ==================================================

// Returns the string corresponding to the type
static const char* typestring(Type* type) {
	if (!type) {
		return "Void";
	}

	switch (type->tag) {
	case TYPE_ID:
		return type->id->name;
	case TYPE_ARRAY: {
		unsigned int counter = 0;
		for (; type->tag == TYPE_ARRAY; type = type->array, counter++);
		const char* id = typestring(type);
		char* str;
		size_t len = 2 * counter + strlen(id);
		MALLOC_ARRAY(str, char, len + 1);
		strcpy(str + counter, id);
		for (int i = 0; i < counter; i++) {
			str[i] = '[';
			str[len - i - 1] = ']';
		}
		str[len] = '\0';
		return str;
	}
	case TYPE_MONITOR:
		return type->monitor.id->name;
	}
	return assert(NULL), NULL; // unreachable
}

// Creates a formatted error with one dynamic string
static const char* err1(const char* format, const char* name) {
	char* err;
	size_t len = strlen(format) - 2 + strlen(name); // -2 for '%s'
	MALLOC_ARRAY(err, char, len + 1);
	sprintf(err, format, name);
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
