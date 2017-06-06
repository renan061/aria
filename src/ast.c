#include <assert.h>
#include <string.h>
#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "alloc.h"
#include "scanner.h" // for primitive_types

// ==================================================
//
//	Program
//
// ==================================================

Program* program;

Program* ast_program(Body* body) {
	assert(body->tag == BODY);
	Program* program;
	MALLOC(program, Program);
	program->body = body;
	return program;
}

// ==================================================
//
//	Body
//
// ==================================================

Body* ast_body(Body* body) {
	Body* base;
	MALLOC(base, Body);
	base->tag = BODY;
	base->next = body;
	return base;
}

Body* ast_body_declaration(Declaration* declaration) {
	Body* body;
	MALLOC(body, Body);
	body->tag = BODY_DECLARATION;
	body->next = NULL;
	body->declaration = declaration;
	return body;
}

Body* ast_body_definition(Definition* definition) {
	Body* body;
	MALLOC(body, Body);
	body->tag = BODY_DEFINITION;
	body->next = NULL;
	body->definition = definition;
	return body;
}

// ==================================================
//
//	Declaration
//
// ==================================================

Declaration* ast_declaration_variable(Variable* variable) {
	Declaration* declaration;
	MALLOC(declaration, Declaration);
	declaration->tag = DECLARATION_VARIABLE;
	declaration->next = NULL;
	declaration->llvm_value = NULL;
	declaration->variable = variable;
	return declaration;
}

Declaration* ast_declaration_function(Id* id, Declaration* params, Type* type) {
	Declaration* declaration;
	MALLOC(declaration, Declaration);
	declaration->tag = DECLARATION_FUNCTION;
	declaration->next = NULL;
	declaration->llvm_value = NULL;
	declaration->function.id = id;
	declaration->function.parameters = params;
	declaration->function.type = type;
	return declaration;
}

// ==================================================
//
//	Definition
//
// ==================================================

Definition* ast_definition_variable(Declaration* declaration, Expression* exp) {
	assert(declaration->tag == DECLARATION_VARIABLE);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_VARIABLE;
	definition->variable.declaration = declaration;
	definition->variable.expression = exp;
	return definition;
}

Definition* ast_definition_function(Declaration* declaration, Block* block) {
	assert(declaration->tag == DECLARATION_FUNCTION);
	assert(block->tag == BLOCK);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_FUNCTION;
	definition->function.declaration = declaration;
	definition->function.block = block;
	return definition;
}

Definition* ast_definition_method(Definition* function, bool private) {
	assert(function->tag == DEFINITION_FUNCTION);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_METHOD;
	definition->method.function = function;
	definition->method.private = private;
	return definition;
}

Definition* ast_definition_constructor(Declaration* declaration, Block* block) {
	assert(declaration->tag == DECLARATION_FUNCTION);
	assert(block->tag == BLOCK);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_CONSTRUCTOR;
	definition->function.declaration = declaration;
	definition->function.block = block;
	return definition;
}

Definition* ast_definition_type(Type* type) {
	assert(type->tag == TYPE_MONITOR);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_TYPE;
	definition->type = type;
	return definition;
}

// ==================================================
//
//	Id
//
// ==================================================

Id* ast_id(unsigned int line, const char* name) {
	Id* id;
	MALLOC(id, Id);
	id->line = line;
	id->name = name;
	return id;
}

// ==================================================
//
//	Type
//
// ==================================================

// TODO: Test static
#define PRIMITIVE_TYPE(v, i)					\
	static Type* v = NULL;						\
	if (!v) {									\
		MALLOC(v, Type);						\
		v->tag = TYPE_ID;						\
		v->primitive = true;					\
		v->immutable = true;					\
		v->id = ast_id(-1, primitive_types[i]);	\
	}											\
	return v;									\

Type* ast_type_void(void) {
	static Type* type_void = NULL;
	if (!type_void) {
		MALLOC(type_void, Type);
		type_void->tag = TYPE_VOID;
		type_void->primitive = true;
		type_void->immutable = true;
		type_void->id = NULL; // just in case
	}
	return type_void;
}

Type* ast_type_boolean(void) {
	PRIMITIVE_TYPE(type_boolean, SCANNER_BOOLEAN);
}

Type* ast_type_integer(void) {
	PRIMITIVE_TYPE(type_integer, SCANNER_INTEGER);
}

Type* ast_type_float(void) {
	PRIMITIVE_TYPE(type_float, SCANNER_FLOAT);
}

Type* ast_type_string(void) {
	PRIMITIVE_TYPE(type_string, SCANNER_STRING);
}

static Type* checkprimitive(Id* id) {
	#define CHECK_TYPE(i, v)					\
		if (primitive_types[i] == id->name) {	\
			return free(id), v;					\
		}										\

	CHECK_TYPE(SCANNER_BOOLEAN, ast_type_boolean());
	CHECK_TYPE(SCANNER_INTEGER, ast_type_integer());
	CHECK_TYPE(SCANNER_FLOAT, ast_type_float());
	CHECK_TYPE(SCANNER_STRING, ast_type_string());
	return NULL;
}

Type* ast_type_id(Id* id) {
	Type* type;
	if ((type = checkprimitive(id))) {
		return type;
	}

	MALLOC(type, Type);
	type->tag = TYPE_ID;
	type->primitive = false;
	type->immutable = false;
	type->id = id;
	return type;
}

Type* ast_type_array(Type* type) {
	Type* arrayType;
	MALLOC(arrayType, Type);
	arrayType->tag = TYPE_ARRAY;
	arrayType->primitive = false;
	arrayType->immutable = false;
	arrayType->array = type;
	return arrayType;
}

Type* ast_type_monitor(Id* id, Body* body) {
	Type* type;
	MALLOC(type, Type);
	type->tag = TYPE_MONITOR;
	type->primitive = false;
	type->immutable = false;
	type->monitor.id = id;
	type->monitor.body = body;
	return type;
}

// ==================================================
//
//	Block
//
// ==================================================

Block* ast_block(Line ln, Block* block) {
	Block* base;
	MALLOC(base, Block);
	base->tag = BLOCK;
	base->line = ln;
	base->next = block;
	return base;
}

Block* ast_block_declaration(Declaration* declaration) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_DECLARATION;
	block->line = 0; // should not be accessed
	block->next = NULL;
	block->declaration = declaration;
	return block;
}

Block* ast_block_definition(Definition* definition) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_DEFINITION;
	block->line = 0; // should not be accessed
	block->next = NULL;
	block->definition = definition;
	return block;
}

Block* ast_block_statement(Statement* statement) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_STATEMENT;
	block->line = statement->line;
	block->next = NULL;
	block->statement = statement;
	return block;
}

// ==================================================
//
//	Statement
//
// ==================================================

Statement* ast_statement_assignment(Line ln, Variable* var, Expression* exp) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_ASSIGNMENT;
	statement->line = ln;
	statement->assignment.variable = var;
	statement->assignment.expression = exp;
	return statement;
}

Statement* ast_statement_function_call(FunctionCall* function_call) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_FUNCTION_CALL;
	statement->line = function_call->line;
	statement->function_call = function_call;
	return statement;
}

Statement* ast_statement_while_wait(Line ln, Expression* exp, Variable* var) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WHILE_WAIT;
	statement->line = ln;
	statement->while_wait.expression = exp;
	statement->while_wait.variable = var;
	return statement;
}

Statement* ast_statement_signal(Line ln, Variable* variable) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SIGNAL;
	statement->line = ln;
	statement->signal = variable;
	return statement;
}

Statement* ast_statement_broadcast(Line ln, Variable* variable) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_BROADCAST;
	statement->line = ln;
	statement->broadcast = variable;
	return statement;
}

Statement* ast_statement_return(Line ln, Expression* expression) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_RETURN;
	statement->line = ln;
	statement->return_ = expression;
	return statement;
}

Statement* ast_statement_if(Line ln, Expression* expression, Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF;
	statement->line = ln;
	statement->if_.expression = expression;
	statement->if_.block = block;
	return statement;
}

// c -> condition, i -> if_block, e -> else_block
Statement* ast_statement_if_else(Line ln, Expression* c, Block* i, Block* e) {
	assert(i->tag == BLOCK && e->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF_ELSE;
	statement->line = ln;
	statement->if_else.expression = c;
	statement->if_else.if_block = i;
	statement->if_else.else_block = e;
	return statement;
}

Statement* ast_statement_while(Line ln, Expression* expression, Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WHILE;
	statement->line = ln;
	statement->while_.expression = expression;
	statement->while_.block = block;
	return statement;
}

Statement* ast_statement_spawn(Line ln, Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SPAWN;
	statement->line = ln;
	statement->spawn = block;
	return statement;
}

Statement* ast_statement_block(Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_BLOCK;
	statement->line = 0; // TODO
	statement->block = block;
	return statement;
}

// ==================================================
//
//	Variable
//
// ==================================================

Variable* ast_variable_id(Id* id) {
	Variable* variable;
	MALLOC(variable, Variable);
	variable->tag = VARIABLE_ID;
	variable->line = id->line;
	variable->type = NULL;
	variable->global = false;
	variable->value = false;
	variable->id = id;
	return variable;
}

Variable* ast_variable_indexed(Line ln, Expression* array, Expression* index) {
	Variable* variable;
	MALLOC(variable, Variable);
	variable->tag = VARIABLE_INDEXED;
	variable->line = ln;
	variable->type = NULL;
	variable->global = false;
	variable->value = false;
	variable->indexed.array = array;
	variable->indexed.index = index;
	return variable;
}

// ==================================================
//
//	Expression
//
// ==================================================

Expression* ast_expression_literal_boolean(Line ln, bool literal_boolean) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_BOOLEAN;
	expression->line = ln;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->literal_boolean = literal_boolean;
	return expression;
}

Expression* ast_expression_literal_integer(Line ln, int literal_integer) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_INTEGER;
	expression->line = ln;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->literal_integer = literal_integer;
	return expression;
}

Expression* ast_expression_literal_float(Line ln, double literal_float) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_FLOAT;
	expression->line = ln;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->literal_float = literal_float;
	return expression;
}

Expression* ast_expression_literal_string(Line ln, const char* literal_string) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_STRING;
	expression->line = ln;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->literal_string = literal_string;
	return expression;
}

Expression* ast_expression_variable(Variable* variable) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_VARIABLE;
	expression->line = variable->line;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->variable = variable;
	return expression;
}

Expression* ast_expression_function_call(FunctionCall* function_call) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_FUNCTION_CALL;
	expression->line = function_call->line;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->function_call = function_call;
	return expression;
}

Expression* ast_expression_unary(Line ln, Token token, Expression* expression) {
	Expression* unaryExpression;
	MALLOC(unaryExpression, Expression);
	unaryExpression->tag = EXPRESSION_UNARY;
	unaryExpression->line = ln;
	unaryExpression->previous = unaryExpression->next = NULL;
	unaryExpression->type = NULL;
	unaryExpression->temp = NULL;
	unaryExpression->unary.token = token;
	unaryExpression->unary.expression = expression;
	return unaryExpression;
}

Expression* ast_expression_binary(Line ln, Token t, Expression* l, Expression* r) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_BINARY;
	expression->line = ln;
	expression->previous = expression->next = NULL;
	expression->type = NULL;
	expression->temp = NULL;
	expression->binary.token = t;
	expression->binary.left_expression = l;
	expression->binary.right_expression = r;
	return expression;
}

Expression* ast_expression_cast(Expression* expression, Type* type) {
	// TODO: Remove when 'as' gets in the language
	assert(expression->type != type);

	Expression* castExpression;
	MALLOC(castExpression, Expression);
	castExpression->tag = EXPRESSION_CAST;
	castExpression->line = expression->line;
	castExpression->type = type;
	castExpression->temp = NULL;
	castExpression->cast = expression;

	// Rearranging the list (only for arguments)
	castExpression->previous = expression->previous;
	expression->previous = NULL;
	castExpression->next = expression->next;
	expression->next = NULL;
	if (castExpression->previous) {
		castExpression->previous->next = castExpression;
	}
	if (castExpression->next) {
		castExpression->next->previous = castExpression;
	}

	return castExpression;
}

// ==================================================
//
//	Function Call
//
// ==================================================

FunctionCall* ast_call(Line ln, Id* id, Expression* arguments) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_BASIC;
	function_call->line = ln;
	function_call->type = NULL;
	function_call->arguments = arguments;
	function_call->declaration = NULL;
	function_call->basic = id;
	return function_call;
}

FunctionCall* ast_call_method(Line ln, Expression* o, Id* n, Expression* args) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_METHOD;
	function_call->line = ln;
	function_call->type = NULL;
	function_call->arguments = args;
	function_call->declaration = NULL;
	function_call->method.object = o;
	function_call->method.name = n;
	return function_call;
}

FunctionCall* ast_call_constructor(Line ln, Type* t, Expression* args) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
	function_call->line = ln;
	function_call->type = NULL;
	function_call->arguments = args;
	function_call->declaration = NULL;
	function_call->constructor = t;
	return function_call;
}
