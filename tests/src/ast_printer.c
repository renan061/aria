#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "ast.h"
#include "parser.h"
#include "scanner.h"

// Does not print types by default
static bool should_print_types = false;

// For identation purposes
static unsigned int tabs = 0;
static void identation(void);

// Prints a scanner token from an expression
static void printtoken(Token);

// Prints a representation of the type
static void printtype(Type*);

static void print_ast_definition(Definition*);
static void print_ast_id(Id*);
static void print_ast_type(Type*);
static void print_ast_block(Block*);
static void print_ast_statement(Statement*);
static void print_ast_variable(Variable*);
static void print_ast_expression(Expression*);
static void print_ast_function_call(FunctionCall*);

void print_ast(AST* ast, bool print_types) {
	should_print_types = print_types;
	for (Definition* d = ast->definitions; d; d = d->next) {
		print_ast_definition(d);
		if (d->tag == DEFINITION_VARIABLE) {
			printf("\n");
		}
	}
}

static void print_ast_definition(Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE: {
		Variable* variable = definition->variable.variable;
		printf("%s ", (variable->value) ? "value" : "variable");
		print_ast_id(variable->id);
		printf(": ");
		if (variable->type) {
			print_ast_type(variable->type);
		} else {
			printf("?");
		}
		if (definition->variable.expression) {
			printf(" = ");
			print_ast_expression(definition->variable.expression);
		}
		break;
	}
	case DEFINITION_FUNCTION:
	case DEFINITION_METHOD:
	case DEFINITION_CONSTRUCTOR:
		if (definition->function.private) {
			assert(definition->tag == DEFINITION_METHOD);
			printf("private ");
		}
		if (definition->function.id) {
			printf("function ");
			print_ast_id(definition->function.id);
		} else {
			printf("initializer");
		}
		if (definition->function.parameters) {
			printf("(");
			for (Definition* p = definition->function.parameters; p;) {
				print_ast_definition(p);
				if ((p = p->next)) {
					printf(", ");
				}
			}
			printf(")");
		}
		if (definition->function.type) {
			printf(": ");
			print_ast_type(definition->function.type);
		} else {
			printtype(definition->function.type);
		}
		printf(" ");
		print_ast_block(definition->function.block);
		printf("\n");
		break;
	case DEFINITION_TYPE:
		printf("monitor ");
		print_ast_id(definition->type->monitor.id);
		printf(" {\n");
		tabs++;
		for (Definition* d = definition->type->monitor.definitions; d;) {
			identation();
			print_ast_definition(d);
			if (d->tag == DEFINITION_VARIABLE) {
				printf("\n");
			}
			d = d->next;
		}
		tabs--;
		printf("}\n");
		break;
	}
}

static void print_ast_id(Id* id) {
	printf("%s", id->name);
}

static void print_ast_type(Type* type) {
	if (type->immutable && !(type->primitive)) {
		printf("Immutable ");
	}
	switch (type->tag) {
	case TYPE_VOID:
		printf("Void");
		break;
	case TYPE_ID:
		print_ast_id(type->id);
		break;
	case TYPE_ARRAY:
		printf("[");
		print_ast_type(type->array);
		printf("]");
		break;
	case TYPE_MONITOR:
		print_ast_id(type->monitor.id);
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
		case BLOCK_DEFINITION:
			identation();
			print_ast_definition(b->definition);
			printf("\n");
			for (Definition* d = b->definition->next; d; d = d->next) {
				assert(d->tag == DEFINITION_VARIABLE);
				identation();
				print_ast_definition(d);
				printf("\n");
			}
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
	case STATEMENT_FUNCTION_CALL:
		print_ast_function_call(statement->function_call);
		printtype(statement->function_call->type);
		break;
	case STATEMENT_WAIT_FOR_IN:
		printf("wait for ");
		print_ast_expression(statement->wait_for_in.condition);
		printf(" in ");
		print_ast_expression(statement->wait_for_in.queue);
		break;
	case STATEMENT_SIGNAL:
		printf("signal ");
		print_ast_expression(statement->signal);
		break;
	case STATEMENT_BROADCAST:
		printf("broadcast ");
		print_ast_expression(statement->broadcast);
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
		print_ast_block(statement->if_.block);
		break;
	case STATEMENT_IF_ELSE:
		printf("if ");
		print_ast_expression(statement->if_else.expression);
		printf(" ");
		print_ast_block(statement->if_else.if_block);
		printf(" else ");
		print_ast_block(statement->if_else.else_block);
		break;
	case STATEMENT_WHILE:
		printf("while ");
		print_ast_expression(statement->while_.expression);
		printf(" ");
		print_ast_block(statement->while_.block);
		break;
	case STATEMENT_SPAWN: {
		Definition* function = statement->spawn->function_definition;
		
		printf("spawn function");
		if (function->function.parameters) {
			printf("(");
			for (Definition* p = function->function.parameters; p;) {
				print_ast_definition(p);
				if ((p = p->next)) {
					printf(", ");
				}
			}
			printf(")");
		}
		printf(" ");
		print_ast_block(function->function.block);
		printf("(");
		if (statement->spawn->arguments) {
			for (Expression* e = statement->spawn->arguments; e;) {
				print_ast_expression(e);
				if ((e = e->next)) {
					printf(", ");
				}
			}
		}
		printf(")");
		break;
	}
	case STATEMENT_BLOCK:
		print_ast_block(statement->block);
		break;
	}

	printf("\n");
}

static void print_ast_variable(Variable* variable) {
	switch (variable->tag) {
	case VARIABLE_ID:
		print_ast_id(variable->id);
		printtype(variable->type);
		break;
	case VARIABLE_INDEXED:
		printf("(");
		print_ast_expression(variable->indexed.array);
		printf("[");
		print_ast_expression(variable->indexed.index);
		printf("]");
		if (variable->indexed.array->type) {
			printtype(variable->indexed.array->type->array);
		}
		printf(")");
		break;
	}
}

static void print_ast_expression(Expression* expression) {
	printf("(");

	switch (expression->tag) {
	case EXPRESSION_LITERAL_BOOLEAN:
		printf("%s", (expression->literal.boolean) ? "true" : "false");
		printtype(expression->type);
		break;
	case EXPRESSION_LITERAL_INTEGER:
		printf("%d", expression->literal.integer);
		printtype(expression->type);
		break;
	case EXPRESSION_LITERAL_FLOAT:
		printf("%f", expression->literal.float_);
		printtype(expression->type);
		break;
	case EXPRESSION_LITERAL_STRING:
		printf("\"%s\"", expression->literal.string);
		printtype(expression->type);
		break;
	case EXPRESSION_LITERAL_ARRAY:
		printf("%s[", expression->literal.immutable ? "Immutable " : "");
		for (Expression* e = expression->literal.array; e;) {
			print_ast_expression(e);
			if ((e = e->next)) {
				printf(", ");
			}
		}
		printf("]");
		printtype(expression->type);
		break;
	case EXPRESSION_VARIABLE:
		print_ast_variable(expression->variable);
		break;
	case EXPRESSION_FUNCTION_CALL:
		print_ast_function_call(expression->function_call);
		printtype(expression->type);
		break;
	case EXPRESSION_UNARY:
		printtoken(expression->unary.token);
		print_ast_expression(expression->unary.expression);
		printtype(expression->type);
		break;
	case EXPRESSION_BINARY:
		print_ast_expression(expression->binary.left_expression);
		printf(" ");
		printtoken(expression->binary.token);
		printtype(expression->type);
		printf(" ");
		print_ast_expression(expression->binary.right_expression);
		break;
	case EXPRESSION_CAST:
		if (!should_print_types) {
			assert(expression->tag != EXPRESSION_CAST);
		}
		print_ast_expression(expression->cast);
		printf(" as");
		printtype(expression->type);
		break;
	}

	printf(")");
}

static void print_ast_function_call(FunctionCall* function_call) {
	switch (function_call->tag) {
	case FUNCTION_CALL_BASIC:
		print_ast_id((function_call->id)
			? function_call->id
			: function_call->function_definition->function.id
		);
		break;
	case FUNCTION_CALL_METHOD:
		print_ast_expression(function_call->instance);
		printf(".");
		print_ast_id((function_call->id)
			? function_call->id
			: function_call->function_definition->function.id
		);
		break;
	case FUNCTION_CALL_CONSTRUCTOR:
		print_ast_type(function_call->type);
	}

	printf("(");
	for (Expression* e = function_call->arguments; e;) {
		print_ast_expression(e);
		if ((e = e->next)) {
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

static void identation(void) {
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

static void printtype(Type* type) {
	if (!should_print_types) {
		return;
	}

	printf(": ");
	print_ast_type(type);
}
