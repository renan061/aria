/*
 * TODO:
 *
 *	- initializer(): Semântica? Pode ter mais de um?
 *		Pode ter mais de um com tipos / quantidade
 *		diferentes de parâmetros?
 *
 *	- Conferir se os tipos existem antes de inserir
 *		eles (ex.: variable: B -> B não existe).
 *
 */
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "errs.h"
#include "parser.h" // for the tokens
#include "symtable.h"

#define ERR_REDECLARATION(id) "symbol redeclaration" // TODO: Use id

// TODO: Remove and deal the with errors
static void todoerr(const char* err) {
	printf("%s\n", err);
	exit(1);
}

// TODO
static void assignment(Variable*, Expression**);

// Auxiliary functions to deal with type analysis
static void typecheck(Type*, Expression**);
static Type* typecast(Expression**, Expression**);
static bool indextype(Type*);
static bool numbertype(Type*);
static bool conditiontype(Type*);
static bool equatabletype(Type*);

// TODO: Doc
static void sem_body(Body*);
static void sem_declaration(Declaration*);
static void sem_definition(Definition*);
static void sem_block(Block*, Type*);
static void sem_statement(Statement*, Type*);
static void sem_variable(Variable*);
static void sem_expression(Expression*);
static void sem_function_call(FunctionCall*);

/*
 * TODO
 */
static SymbolTable* table;

// TODO: Doc
void sem_analyse(Program* program) {
	table = symtable_new();
	sem_body(program->body);
	symtable_free(table);
}

// ==================================================
//
//	Recursive functions
//
// ==================================================

static void sem_body(Body* body) {
	if (body->tag == BODY) {
		if (body->next) {
			symtable_enter_scope(table);
			sem_body(body->next);
			symtable_leave_scope(table);
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
		if (!symtable_insert(table, declaration)) {
			sem_error(declaration->variable->id->line,
				ERR_REDECLARATION(declaration->variable->id->name));
		}
		break;
	case DECLARATION_FUNCTION:
		if (declaration->function.id) { // constructors have id == NULL
			if (!symtable_insert(table, declaration)) {
				sem_error(declaration->function.id->line,
					ERR_REDECLARATION(declaration->function.id->name));
			}
		}
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
		symtable_enter_scope(table);
		sem_block(definition->function.block,
			definition->function.declaration->function.type);
		symtable_leave_scope(table);
		break;
	case DEFINITION_METHOD:
		// TODO: semantics? (private)
		sem_definition(definition->method.function);
		break;
	case DEFINITION_MONITOR:
		// TODO: utable: how to insert a monitor->id? create DECLARATION_MONITOR?

		// if (!symtable_insert(utable, definition->monitor.id)) {
		// 	sem_error(definition->monitor.id->line,
		// 		ERR_REDECLARATION(definition->monitor.id->name));
		// }
		// sem_body(definition->monitor.body);
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
			todoerr("invalid type for condition");
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
			todoerr("invalid type for condition");
		}
		symtable_enter_scope(table);
		sem_block(statement->if_.block, return_type);
		symtable_leave_scope(table);
		break;
	case STATEMENT_IF_ELSE:
		sem_expression(statement->if_else.expression);
		if (!conditiontype(statement->if_else.expression->type)) {
			todoerr("invalid type for condition");
		}
		symtable_enter_scope(table);
		sem_block(statement->if_else.if_block, return_type);
		symtable_leave_scope(table);
		symtable_enter_scope(table);
		sem_block(statement->if_else.else_block, return_type);
		symtable_leave_scope(table);
		break;
	case STATEMENT_WHILE:
		sem_expression(statement->while_.expression);
		if (!conditiontype(statement->while_.expression->type)) {
			todoerr("invalid type for condition");
		}
		symtable_enter_scope(table);
		sem_block(statement->while_.block, return_type);
		symtable_leave_scope(table);
		break;
	case STATEMENT_SPAWN:
		symtable_enter_scope(table);
		// TODO: Special semantics for spawn blocks
		sem_block(statement->spawn, return_type);
		symtable_leave_scope(table);
		break;
	case STATEMENT_BLOCK:
		symtable_enter_scope(table);
		sem_block(statement->block, return_type);
		symtable_leave_scope(table);
		break;
	}
}

static void sem_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID: {
		Declaration* declaration = symtable_find(table, variable->id);
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
		expression->type = ast_type_boolean();
		break;
	case EXPRESSION_LITERAL_INTEGER:
		expression->type = ast_type_integer();
		break;
	case EXPRESSION_LITERAL_FLOAT:
		expression->type = ast_type_float();
		break;
	case EXPRESSION_LITERAL_STRING:
		expression->type = ast_type_string();
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
			if (!numbertype(expression->unary.expression->type)) {
				todoerr("invalid type for unary minus");
			}
			expression->type = expression->unary.expression->type;
			break;
		case TK_NOT:
			if (!conditiontype(expression->unary.expression->type)) {
				todoerr("invalid type for unary not");
			}
			expression->type = ast_type_boolean();
			break;
		}
		break;
	case EXPRESSION_BINARY:
		sem_expression(expression->binary.left_expression);
		sem_expression(expression->binary.right_expression);
		switch (expression->binary.token) {
		case TK_OR: case TK_AND:
			if (!conditiontype(expression->binary.left_expression->type)) {
				todoerr("invalid type for left and/or expression");
			}
			if (!conditiontype(expression->binary.right_expression->type)) {
				todoerr("invalid type for left and/or expression");
			}
			expression->type = ast_type_boolean();
			break;
		case TK_EQUAL:
			if (!equatabletype(expression->binary.right_expression->type)) {
				todoerr("invalid type for left comparison expression");
			}
			if (!equatabletype(expression->binary.right_expression->type)) {
				todoerr("invalid type for right comparison expression");
			}
			typecast(&expression->binary.left_expression,
				&expression->binary.right_expression);
			expression->type = ast_type_boolean();
			break;
		case TK_LEQUAL: case TK_GEQUAL: case '<': case '>':
			if (!numbertype(expression->binary.right_expression->type)) {
				todoerr("invalid type for left comparison expression");
			}
			if (!numbertype(expression->binary.right_expression->type)) {
				todoerr("invalid type for right comparison expression");
			}
			typecast(&expression->binary.left_expression,
				&expression->binary.right_expression);
			expression->type = ast_type_boolean();
			break;
		case '+': case '-': case '*': case '/':
			if (!numbertype(expression->binary.right_expression->type)) {
				todoerr("invalid type for left arith expression");
			}
			if (!numbertype(expression->binary.right_expression->type)) {
				todoerr("invalid type for right arith expression");
			}
			expression->type = typecast(&expression->binary.left_expression,
				&expression->binary.right_expression);
			break;
		}
		break;
	case EXPRESSION_CAST:
		assert(expression->tag != EXPRESSION_CAST);
		break;
	}
}

static void sem_function_call(FunctionCall* function_call) {
	Declaration* declaration;

	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC:
		declaration = symtable_find(table, function_call->basic);
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
		// TODO

		// Check if function_call->constructor->id->tag
		// if id (monitor constructor)
		// 		find monitor type in symtable
		// if array (array constructor)
		// 		find array "final type" in symtable

		break;
	}

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
static void assignment(Variable* variable, Expression** expression) {
	if (!(*expression)->type) {
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
	// TODO
}

/*
 * Casts one of the expressions if they have mismatching types.
 * Casts always go from integer to float.
 * The expressions must be of number type.
 */
static Type* typecast(Expression** le, Expression** re) {
	assert(numbertype((*le)->type) && numbertype((*re)->type));

	if ((*le)->type == (*re)->type) {
		return (*le)->type;
	}

	Type* float_ = ast_type_float();
	if ((*le)->type == float_) {
		return (*re = ast_expression_cast(*re, float_))->type;
	}
	if ((*re)->type == float_) {
		return (*le = ast_expression_cast(*le, float_))->type;
	}

	return NULL;
}

static bool indextype(Type* type) {
	return type == ast_type_integer();
}

static bool numbertype(Type* type) {
	return type == ast_type_integer() || type == ast_type_float();
}

static bool conditiontype(Type* type) { // TODO: Necessary?
	return type == ast_type_boolean();
}

static bool equatabletype(Type* type) { // TODO: Strings?
	return type == ast_type_boolean()
		|| type == ast_type_integer()
		|| type == ast_type_float();
}
