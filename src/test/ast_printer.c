#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "parser.h"
#include "scanner.h"

// For identation purposes
static unsigned int tabs = 0;
static void identation();

// Prints a scanner token from an expression
static void printtoken(Token token);

static void print_ast_body(Body*);
static void print_ast_declaration(Declaration*);
static void print_ast_definition(Definition*);
static void print_ast_id(Id*);
static void print_ast_type(Type*);
static void print_ast_block(Block*);
static void print_ast_statement(Statement*);
static void print_ast_variable(Variable*);
static void print_ast_expression(Expression*);
static void print_ast_function_call(FunctionCall*);

void print_ast_program(Program* program) {
	assert(program->body->tag == BODY);
	print_ast_body(program->body);
	printf("\n");
}

static void print_ast_body(Body* body) {
	if (body->tag == BODY) {
		printf("{\n");
		if (body->next) {
			tabs++;
			print_ast_body(body->next);
			tabs--;
		}
		identation();
		printf("}");
		return;
	}

	for (Body* b = body; b; b = b->next) {
		identation();
		switch (b->tag) {
		case BODY_DECLARATION:
			print_ast_declaration(b->declaration);
			printf("\n");
			break;
		case BODY_DEFINITION:
			print_ast_definition(b->definition);
			break;
		default:
			assert(b->tag != BODY);
		}
	}
}

static void print_ast_declaration(Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		print_ast_id(declaration->variable.id);
		printf(": ");
		if (declaration->variable.type) { // for := statements
			print_ast_type(declaration->variable.type);
		} else {
			printf("?");
		}
		break;
	case DECLARATION_FUNCTION:
		if (declaration->function.id) {
			printf("function ");
			print_ast_id(declaration->function.id);
		} else {
			printf("initializer");
		}
		printf("(");
		for (Declaration* p = declaration->function.parameters; p;) {
			print_ast_declaration(p);
			if ((p = p->next)) {
				printf(", ");
			}
		}
		printf(")");
		if (declaration->function.type) {
			printf(": ");
			print_ast_type(declaration->function.type);
		}
		break;
	}
}

static void print_ast_definition(Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE:
		print_ast_declaration(definition->variable.declaration);
		printf(" = ");
		print_ast_expression(definition->variable.expression);
		break;
	case DEFINITION_FUNCTION:
	case DEFINITION_CONSTRUCTOR:
		print_ast_declaration(definition->function.declaration);
		printf(" ");
		assert(definition->function.block->tag == BLOCK);
		print_ast_block(definition->function.block);
		printf("\n");
		break;
	case DEFINITION_METHOD:
		if (definition->method.private) {
			printf("private ");
		}
		print_ast_definition(definition->method.function);
		break;
	case DEFINITION_MONITOR:
		printf("monitor ");
		print_ast_id(definition->monitor.id);
		printf(" ");
		assert(definition->monitor.body->tag == BODY);
		print_ast_body(definition->monitor.body);
		printf("\n");
		break;
	}
}

static void print_ast_id(Id* id) {
	// TODO: Declaration
	printf("%s", id->name);
}

static void print_ast_type(Type* type) {
	switch (type->tag) {
	case TYPE_ID:
		print_ast_id(type->id);
		break;
	case TYPE_ARRAY:
		printf("[");
		print_ast_type(type->array);
		printf("]");
		break;
	}
}

static void print_ast_block(Block* block) {
	if (block->tag == BLOCK) {
		printf("{\n");
		if (block->next) {
			tabs++;
			print_ast_block(block->next);
			tabs--;
		}
		identation();
		printf("}");
		return;
	}

	for (Block* b = block; b; b = b->next) {
		switch (b->tag) {
		case BLOCK_DECLARATION:
			identation();
			print_ast_declaration(b->declaration);
			printf("\n");
			break;
		case BLOCK_STATEMENT:
			print_ast_statement(b->statement);
			break;
		default:
			assert(b->tag != BLOCK);
		}
	}
}

static void print_ast_statement(Statement* statement) {
	identation();

	switch (statement->tag) {
	case STATEMENT_ASSIGNMENT:
		print_ast_variable(statement->assignment.variable);
		printf(" = ");
		print_ast_expression(statement->assignment.expression);
		break;
	case STATEMENT_DEFINITION:
		print_ast_declaration(statement->definition.declaration);
		printf(" := ");
		print_ast_expression(statement->definition.expression);
		break;
	case STATEMENT_FUNCTION_CALL:
		print_ast_function_call(statement->function_call);
		break;
	case STATEMENT_WHILE_WAIT:
		printf("while ");
		print_ast_expression(statement->while_wait.expression);
		printf(" wait in ");
		print_ast_variable(statement->while_wait.variable);
		break;
	case STATEMENT_SIGNAL:
		printf("signal ");
		print_ast_variable(statement->signal);
		break;
	case STATEMENT_BROADCAST:
		printf("broadcast ");
		print_ast_variable(statement->broadcast);
		break;
	case STATEMENT_RETURN:
		printf("return");
		if (statement->return_) {
			printf(" ");
			print_ast_expression(statement->return_);
		}
		break;
	case STATEMENT_IF:
		printf("if ");
		print_ast_expression(statement->if_.expression);
		printf(" ");
		assert(statement->if_.block->tag == BLOCK);
		print_ast_block(statement->if_.block);
		break;
	case STATEMENT_IF_ELSE:
		printf("if ");
		print_ast_expression(statement->if_else.expression);
		printf(" ");
		assert(statement->if_else.if_block->tag == BLOCK);
		print_ast_block(statement->if_else.if_block);
		printf(" else ");
		assert(statement->if_else.else_block->tag == BLOCK);
		print_ast_block(statement->if_else.else_block);
		break;
	case STATEMENT_WHILE:
		printf("while ");
		print_ast_expression(statement->while_.expression);
		printf(" ");
		assert(statement->while_.block->tag == BLOCK);
		print_ast_block(statement->while_.block);
		break;
	case STATEMENT_SPAWN:
		printf("spawn ");
		assert(statement->spawn->tag == BLOCK);
		print_ast_block(statement->spawn);
		break;
	case STATEMENT_BLOCK:
		assert(statement->block->tag == BLOCK);
		print_ast_block(statement->block);
		break;
	}

	printf("\n");
}

static void print_ast_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID:
		print_ast_id(variable->id);
		break;
	case VARIABLE_INDEXED:
		print_ast_expression(variable->indexed.array);
		printf("[");
		print_ast_expression(variable->indexed.index);
		printf("]");
		break;
	}
}

static void print_ast_expression(Expression* expression) {
	printf("(");

	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		printf("%s", (expression->literal_boolean) ? "true" : "false");
		break;
	case EXPRESSION_LITERAL_INTEGER:
		printf("%d", expression->literal_integer);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		printf("%f", expression->literal_float);
		break;
	case EXPRESSION_LITERAL_STRING:
		printf("\"%s\"", expression->literal_string);
		break;
	case EXPRESSION_VARIABLE:
		print_ast_variable(expression->variable);
		break;
	case EXPRESSION_FUNCTION_CALL:
		print_ast_function_call(expression->function_call);
		break;
	case EXPRESSION_UNARY:
		printtoken(expression->unary.token);
		print_ast_expression(expression->unary.expression);
		break;
	case EXPRESSION_BINARY:
		print_ast_expression(expression->binary.left_expression);
		printf(" ");
		printtoken(expression->binary.token);
		printf(" ");
		print_ast_expression(expression->binary.right_expression);
		break;
	}

	printf(")");
}

static void print_ast_function_call(FunctionCall* function_call) {
	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC:
		print_ast_id(function_call->basic);
		break;
	case FUNCTION_CALL_METHOD:
		print_ast_expression(function_call->method.object);
		printf(".");
		print_ast_id(function_call->method.name);
		break;
	case FUNCTION_CALL_CONSTRUCTOR:
		print_ast_type(function_call->constructor);
	}

	printf("(");
	for (Expression* e = function_call->arguments; e; e = e->next) {
		print_ast_expression(e);
		if (e->next) {
			printf(", ");
		}
	}
	printf(")");
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

static void identation() {
	for (int i = 0; i < tabs; i++) {
		printf("\t");
	}
}

static void printtoken(Token token) {
	switch (token) {
    case TK_OR:		printf("or");	break;
    case TK_AND:	printf("and");	break;
    case TK_EQUAL:	printf("==");	break;
    case TK_NEQUAL:	printf("!=");	break;
    case TK_LEQUAL:	printf("<=");	break;
    case TK_GEQUAL:	printf(">=");	break;
    case TK_NOT:	printf("not");	break;
    default:		printf("%c", token);
	}
}
