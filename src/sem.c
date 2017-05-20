/*
 * TODO:
 *
 *	- initializer(): Semântica? Pode ter mais de um?
 *		Pode ter mais de um com tipos / quantidade
 *		diferentes de parâmetros?
 *	- Create TYPE_VOID and stop using NULL as Void
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
#define UNREACHABLE assert(NULL);

// TODO: Remove and deal the with errors
static void todoerr(const char* err) {
	printf("%s\n", err);
	exit(1);
}

// Symbol table for declarations (lowercase-table) and types (uppercase-table)
static SymbolTable *ltable, *utable;

// A monitor type if currently analysing a monitor's body, NULL otherwise
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
static void typecheck1(Type*, Expression**);
static Type* typecheck2(Expression**, Expression**);
static bool indextype(Type*);
static bool numerictype(Type*);
static bool conditiontype(Type*);
static bool equatabletype(Type*);

// Auxiliary functions that deal with errors
static void err_redeclaration(Id*);
static void err_unkown_type_name(Id*);
static void err_invalid_condition_type(Expression*);
static void err_type(Line, Type*, Type*);
static void err_assignment_void(Line);
static void err_return_void(Line, Type*);
static void err_variable_unknown(Id*);
static void err_variable_misuse(Id*);
static void err_variable_array_type(Variable*);
static void err_variable_array_index_type(Variable*);

// ==================================================
//
//	TODO: Experimental
//
// ==================================================

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
//	TODO: Experimental
//
// ==================================================

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
			typecheck1(return_type, &statement->return_);
		} else if (return_type) {
			err_return_void(statement->line, return_type);
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
		Declaration* dec = symtable_find_declaration(ltable, variable->id);
		if (!dec) {
			err_variable_unknown(variable->id);
		} else if (dec->tag != DECLARATION_VARIABLE) {
			err_variable_misuse(variable->id);
		}
		free(variable->id);
		variable->id = dec->variable->id;
		variable->type = dec->variable->type;
		break;
	}
	case VARIABLE_INDEXED:
		sem_expression(variable->indexed.array);
		sem_expression(variable->indexed.index);
		if (variable->indexed.array->type->tag != TYPE_ARRAY) {
			err_variable_array_type(variable);
		}
		if (!indextype(variable->indexed.index->type)) {
			err_variable_array_index_type(variable);
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
		}
		break;
	case EXPRESSION_BINARY: {
		Expression** lp = &expression->binary.left_expression;
		Expression** rp = &expression->binary.right_expression;
		sem_expression(*lp);
		sem_expression(*rp);
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
			assert(typecheck2(lp, rp));
			expression->type = boolean_;
			break;
		case '+': case '-': case '*': case '/':
			if (!numerictype((*lp)->type)) {
				err_expression(ERR_EXPRESSION_LEFT, expression);
			}
			if (!numerictype((*rp)->type)) {
				err_expression(ERR_EXPRESSION_RIGHT, expression);
			}
			expression->type = typecheck2(lp, rp);
			assert(expression->type);
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

// TODO
static bool typeequals(Type* type1, Type* type2) {
	if ((type1 && !type2) || (!type1 && type2)) { // TODO: Remove when TYPE_VOID
		return false;
	}
	if (!type1 && !type2) { // TODO: Remove when TYPE_VOID
		return true;
	}

	if (type1 == type2) {
		return true;
	}

	if (type1->tag != TYPE_ARRAY || type2->tag != TYPE_ARRAY) {
		return false;
	}

	Type *t1, *t2;
	int counter1 = 0, counter2 = 0;
	for (t1 = type1; t1->tag == TYPE_ARRAY; t1 = t1->array, counter1++);
	for (t2 = type2; t2->tag == TYPE_ARRAY; t2 = t2->array, counter2++);
	return t1->tag == t2->tag && counter1 == counter2;
}

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
		UNREACHABLE;
	}
}

/*
 * Checks for type equivalence between variable and expression.
 * Works for definitions and simple assignments.
 * Deals with errors internally.
 */
static void assignment(Variable* variable, Expression** expression) {
	if (!(*expression)->type) { // when defining with implicit type
		err_assignment_void((*expression)->line);
	}
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
	default:
		UNREACHABLE;
	}
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

static void err_assignment_void(Line line) {
	sem_error(line, "can't assign a Void expression to a variable");
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
