#include <stdio.h>

#include "ast.h"

static void ast_print_body(Body*);
static void ast_print_declaration(Declaration*);
static void ast_print_definition(Definition*);
static void ast_print_id(Id*);
static void ast_print_type(Type*);
static void ast_print_block(Block*);
static void ast_print_statement(Statement*);
static void ast_print_variable(Variable*);
static void ast_print_expression(Expression*);
static void ast_print_function_call(FunctionCall*);

void ast_print_program(Program* program) {
	for (Body* b = program->body; b; b = b->next) {
		ast_print_body(b);
	}
}

static void ast_print_body(Body* body) {
	switch (body->tag) {
	case BODY_DECLARATION:
		ast_print_declaration(body->declaration);
		break;
	case BODY_DEFINITION:
		ast_print_definition(body->definition);
		break;
	}
}

static void ast_print_declaration(Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		ast_print_id(declaration->variable.id);
		printf(": ");
		ast_print_type(declaration->variable.type);
		break;
	case DECLARATION_FUNCTION:
		printf("function ");
		if (declaration->function.id) {
			ast_print_id(declaration->function.id);
		} else {
			printf("initializer");
		}
		printf("(");
		for (Declaration* p = declaration->function.parameters; p;) {
			ast_print_declaration(p);
			if ((p = p->next)) {
				printf(", ");
			}
		}
		printf(")");
		if (declaration->function.type) {
			ast_print_type(declaration->function.type);
		}
		break;
	}
}

static void ast_print_definition(Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE:
		ast_print_declaration(definition->variable.declaration);
		printf(" = ");
		ast_print_expression(definition->variable.expression);
		break;
	case DEFINITION_FUNCTION:
	case DEFINITION_CONSTRUCTOR:
		ast_print_declaration(definition->function.declaration);
		printf(" {");
		for (Block* b = definition->function.block; b; b = b->next) {
			ast_print_block(b);
			printf("\n");
		}
		printf("}\n");
		break;
	case DEFINITION_METHOD:
		if (definition->method.private) {
			printf("private ");
		}
		ast_print_definition(definition->method.function);
		break;
	case DEFINITION_MONITOR:
		printf("monitor ");
		ast_print_id(definition->monitor.id);
		printf("{\n");
		for (Body* b = definition->monitor.body; b; b = b->next) {
			ast_print_body(b);
		}
		printf("}");
		break;
	}
}

static void ast_print_id(Id* id) {
	// TODO: Declaration
	printf("%s", id->name);
}

static void ast_print_type(Type* type) {
	switch (type->tag) {
	case TYPE_ID:
		ast_print_id(type->id);
		break;
	case TYPE_ARRAY:
		printf("[");
		ast_print_type(type->array);
		printf("]");
		break;
	}
}

static void ast_print_block(Block* block) {
	switch (block->tag) {
	case BLOCK_DECLARATION:
		ast_print_declaration(block->declaration);
		break;
	case BLOCK_STATEMENT:
		ast_print_statement(block->statement);
		break;
	}
}

static void ast_print_statement(Statement* statement) {
	// TODO
}

static void ast_print_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID:
		ast_print_id(variable->id);
		break;
	case VARIABLE_INDEXED:
		ast_print_expression(variable->indexed.array);
		printf("[");
		ast_print_expression(variable->indexed.index);
		printf("]");
		break;
	}
}

static void ast_print_expression(Expression* expression) {
	// TODO
}

static void ast_print_function_call(FunctionCall* function_call) {
	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC:
		ast_print_id(function_call->basic);
		break;
	case FUNCTION_CALL_METHOD:
		ast_print_expression(function_call->method.object);
		printf(".");
		ast_print_id(function_call->method.name);
		break;
	case FUNCTION_CALL_CONSTRUCTOR:
		ast_print_type(function_call->constructor);
	}
}
