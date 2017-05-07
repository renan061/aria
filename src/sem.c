#include <assert.h>
#include <stdbool.h>

#include "ast.h"
#include "errs.h"
#include "symtable.h"

// TODO: Use id
#define ERR_REDECLARATION(id) "symbol redeclaration"

// TODO: Doc
static void sem_body(Body*);
static void sem_declaration(Declaration*);
static void sem_definition(Definition*);
static void sem_id(Id*);
static void sem_type(Type*);
static void sem_block(Block*, Type*);
static void sem_statement(Statement*);
static void sem_variable(Variable*);
static void sem_expression(Expression*);
static void sem_function_call(FunctionCall*);

// TODO: Doc
static SymbolTable* table;

// TODO: Doc
void sem_analyse(Program* program) {
	assert(program->body->tag == BODY);
	table = symtable_new();
	symtable_enter_scope(table);
	sem_body(program->body->next);
	symtable_leave_scope(table);
	symtable_free(table);
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

void sem_body(Body* body) {
	// TODO
}

void sem_declaration(Declaration* declaration) {
	// TODO
}

void sem_definition(Definition* definition) {
	// Auxiliary macro to shorten definition-to-declaration expressions
	// TODO: Rename
	#define _DEFDEC(a, b) definition->a.declaration->a.b

	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Id* id = _DEFDEC(variable, id);
		if (!symtable_insert(table, id)) {
			sem_error(id->line, ERR_REDECLARATION(id->name));
		}
		break;
	}
	case DEFINITION_FUNCTION: {
		Id* id = _DEFDEC(function, id);
		if (!symtable_insert(table, id)) {
			sem_error(id->line, ERR_REDECLARATION(id->name));
		}
		/* fallthrough */
	}
	case DEFINITION_CONSTRUCTOR:
		symtable_enter_scope(table);
		if (_DEFDEC(function, parameters)) {
			sem_declaration(_DEFDEC(function, parameters));
		}
		sem_block(definition->function.block, _DEFDEC(function, type));		
		symtable_leave_scope(table);
		break;
	case DEFINITION_METHOD:
		sem_definition(definition->method.function);
		break;
	case DEFINITION_MONITOR:
		sem_body(definition->monitor.body);
		break;
	}
}

void sem_id(Id* id) {

}

void sem_type(Type* type) {

}

// TODO: Type
void sem_block(Block* block, Type* type) {

}

void sem_statement(Statement* statement) {

}

void sem_variable(Variable* variable) {

}

void sem_expression(Expression* expression) {

}

void sem_function_call(FunctionCall* function_call) {

}
