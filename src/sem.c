/*
 * TODO:
 *
 *	- initializer(): Semântica? Pode ter mais de um? Pode ter mais de um com
 *		tipos / quantidade diferentes de parâmetros?
 *	- Necessidade de ter algo como "type Integer;"?
 *
 */
#include <assert.h>
#include <stdbool.h>
#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "errs.h"
#include "symtable.h"

#define ERR_REDECLARATION(id) "symbol redeclaration" // TODO: Use id

// Checks for type equivalence and deals with errors internally.
static void typecheck(Type*, Expression*);

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
 * Symbol table for lowercase ids (holds variables and functions).
 */
static SymbolTable* ltable;

/*
 * Symbol table for uppercase ids (holds monitors).
 */
static SymbolTable* utable;

// TODO: Doc
void sem_analyse(Program* program) {
	assert(program->body->tag == BODY);
	ltable = symtable_new();
	utable = symtable_new();
	sem_body(program->body);
	symtable_free(ltable);
	symtable_free(utable);
}

static void sem_body(Body* body) {
	if (body->tag == BODY) {
		if (body->next) {
			symtable_enter_scope(ltable);
			sem_body(body->next);
			symtable_leave_scope(ltable);
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
		if (!symtable_insert(ltable, declaration)) {
			sem_error(declaration->variable->id->line,
				ERR_REDECLARATION(declaration->variable->id->name));
		}
		break;
	case DECLARATION_FUNCTION:
		if (declaration->function.id) { // constructors have id == NULL
			if (!symtable_insert(ltable, declaration)) {
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
		sem_declaration(definition->variable.declaration);
		sem_expression(definition->variable.expression);
		typecheck(definition->variable.declaration->variable->type,
			definition->variable.expression);
		break;
	case DEFINITION_FUNCTION:
	case DEFINITION_CONSTRUCTOR:
		symtable_enter_scope(ltable);
		sem_declaration(definition->function.declaration);
		assert(definition->function.block->tag == BLOCK);
		sem_block(definition->function.block,
			definition->function.declaration->function.type);
		symtable_leave_scope(ltable);
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
		// assert(definition->monitor.body->tag == BODY);
		// sem_body(definition->monitor.body);
		break;
	}
}

// TODO: Type
static void sem_block(Block* block, Type* type) {
	if (block->tag == BLOCK) {
		if (block->next) {
			sem_block(block->next, type);
		}
		return;
	}

	for (Block* b = block; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			sem_declaration(b->declaration);
			break;
		case BLOCK_STATEMENT:
			sem_statement(b->statement, type);
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
		typecheck(statement->assignment.variable->type,
			statement->assignment.expression);
		break;
	case STATEMENT_DEFINITION:
		sem_declaration(statement->definition.declaration);
		sem_expression(statement->definition.expression);
		statement->definition.declaration->variable->type =
			statement->definition.expression->type;
		break;
	case STATEMENT_FUNCTION_CALL:
		sem_function_call(statement->function_call);
		break;
	case STATEMENT_WHILE_WAIT:
		sem_expression(statement->while_wait.expression);
		sem_variable(statement->while_wait.variable);
		break;
	case STATEMENT_SIGNAL:
		sem_variable(statement->signal);
		break;
	case STATEMENT_BROADCAST:
		sem_variable(statement->broadcast);
		break;
	case STATEMENT_RETURN:
		if (statement->return_) {
			sem_expression(statement->return_);
			typecheck(return_type, statement->return_);
		}
		break;
	case STATEMENT_IF:
		sem_expression(statement->if_.expression);
		assert(statement->if_.block->tag == BLOCK);
		sem_block(statement->if_.block, return_type);
		break;
	case STATEMENT_IF_ELSE:
		sem_expression(statement->if_else.expression);
		assert(statement->if_else.if_block->tag == BLOCK);
		sem_block(statement->if_else.if_block, return_type);
		assert(statement->if_else.else_block->tag == BLOCK);
		sem_block(statement->if_else.else_block, return_type);
		break;
	case STATEMENT_WHILE:
		sem_expression(statement->while_.expression);
		assert(statement->while_.block->tag == BLOCK);
		sem_block(statement->while_.block, return_type);
		break;
	case STATEMENT_SPAWN:
		assert(statement->spawn->tag == BLOCK);
		sem_block(statement->spawn, return_type);
		break;
	case STATEMENT_BLOCK:
		assert(statement->block->tag == BLOCK);
		sem_block(statement->block, return_type);
		break;
	}
}

static void sem_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID: {
		Declaration* declaration = symtable_find(ltable, variable->id);
		if (!declaration) {
			// TODO: "variable not defined"
			assert(0);
		} else if (declaration->tag != DECLARATION_VARIABLE) {
			// TODO: "not a variable"
			assert(0);
		}
		// TODO: free variable->id ?
		variable->id = declaration->variable->id;
		variable->type = declaration->variable->type;
		break;
	}
	case VARIABLE_INDEXED:
		sem_expression(variable->indexed.array);
		sem_expression(variable->indexed.index);
		if (variable->indexed.array->type->tag != TYPE_ARRAY) {
			// TODO: "not array type"
			assert(0);
		}
		// TODO: error "invalid index type for array"
		// typecheck(type_integer, variable->indexed.index);
		variable->type = variable->indexed.array->type->array;
		break;
	}
}

static void sem_expression(Expression* expression) {
	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		// TODO
		// expression->type = type_boolean;
		break;
	case EXPRESSION_LITERAL_INTEGER:
		// TODO
		// expression->type = type_integer;
		break;
	case EXPRESSION_LITERAL_FLOAT:
		// TODO
		// expression->type = type_float;
		break;
	case EXPRESSION_LITERAL_STRING:
		// TODO
		// expression->type = type_string;
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
		// TODO
		break;
	case EXPRESSION_BINARY:
		// TODO
		break;
	}
}

static void sem_function_call(FunctionCall* function_call) {
	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC: {
		Declaration* declaration = symtable_find(ltable, function_call->basic);
		if (!declaration) {
			// TODO: "function not defined"
			assert(0);
		} else if (declaration->tag != DECLARATION_FUNCTION) {
			// TODO: "not a function"
			assert(0);
		}
		// TODO: free old function_call->basic ?
		function_call->basic = declaration->function.id;
		break;
	}
	case FUNCTION_CALL_METHOD:
		// TODO

		// object.name(...)
		
		// Look inside object->type scope for name

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

static void typecheck(Type* type, Expression* expression) {
	// TODO
}
