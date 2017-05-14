#include <assert.h>
#include <string.h>
#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "alloc.h"

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
	declaration->variable = variable;
	return declaration;
}

Declaration* ast_declaration_function(Id* id, Declaration* params, Type* type) {
	Declaration* declaration;
	MALLOC(declaration, Declaration);
	declaration->tag = DECLARATION_FUNCTION;
	declaration->next = NULL;
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

Definition* ast_definition_monitor(Id* id, Body* body) {
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_MONITOR;
	definition->monitor.id = id;
	definition->monitor.body = body;
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

#define PRIMITIVE_TYPE(v, s)			\
	static Type* v = NULL;				\
	if (!v) {							\
		MALLOC(v, Type);				\
		v->tag = TYPE_ID;				\
		v->primitive = true;			\
		v->id = ast_id(-1, s);			\
	}									\
	return v;							\

Type* ast_type_boolean(void) {
	PRIMITIVE_TYPE(type_boolean, "Boolean");
}

Type* ast_type_integer(void) {
	PRIMITIVE_TYPE(type_integer, "Integer");
}

Type* ast_type_float(void) {
	PRIMITIVE_TYPE(type_float, "Float");
}

Type* ast_type_string(void) {
	PRIMITIVE_TYPE(type_string, "String");
}

static Type* checkprimitive(Id* id) {
	#define CHECK_TYPE(s, v)			\
		if (!strcmp(s, id->name)) {		\
			free(id);					\
			return v;					\
		}								\

	CHECK_TYPE("Boolean", ast_type_boolean());
	CHECK_TYPE("Integer", ast_type_integer());
	CHECK_TYPE("Float", ast_type_float());
	CHECK_TYPE("String", ast_type_string());
	return NULL;
}

Type* ast_type_id(Id* id) {
	Type* type;
	if ((type = checkprimitive(id))) { // For primitive types
		return type;
	}

	MALLOC(type, Type);
	type->tag = TYPE_ID;
	type->primitive = false;
	type->id = id;
	return type;
}

Type* ast_type_array(Type* type) {
	Type* arrayType;
	MALLOC(arrayType, Type);
	arrayType->tag = TYPE_ARRAY;
	arrayType->primitive = false;
	arrayType->array = type;
	return arrayType;	
}

// ==================================================
//
//	Block
//
// ==================================================

Block* ast_block(Block* block) {
	Block* base;
	MALLOC(base, Block);
	base->tag = BLOCK;
	base->next = block;
	return base;
}

Block* ast_block_declaration(Declaration* declaration) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_DECLARATION;
	block->next = NULL;
	block->declaration = declaration;
	return block;
}

Block* ast_block_definition(Definition* definition) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_DEFINITION;
	block->next = NULL;
	block->definition = definition;
	return block;
}

Block* ast_block_statement(Statement* statement) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_STATEMENT;
	block->next = NULL;
	block->statement = statement;
	return block;
}

// ==================================================
//
//	Statement
//
// ==================================================

Statement* ast_statement_assignment(Variable* var, Expression* exp) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_ASSIGNMENT;
	statement->assignment.variable = var;
	statement->assignment.expression = exp;
	return statement;
}

Statement* ast_statement_function_call(FunctionCall* function_call) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_FUNCTION_CALL;
	statement->function_call = function_call;
	return statement;
}

Statement* ast_statement_while_wait(Expression* exp, Variable* var) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WHILE_WAIT;
	statement->while_wait.expression = exp;
	statement->while_wait.variable = var;
	return statement;
}

Statement* ast_statement_signal(Variable* variable) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SIGNAL;
	statement->signal = variable;
	return statement;
}

Statement* ast_statement_broadcast(Variable* variable) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_BROADCAST;
	statement->broadcast = variable;
	return statement;
}

Statement* ast_statement_return(Expression* expression) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_RETURN;
	statement->return_ = expression;
	return statement;
}

Statement* ast_statement_if(Expression* expression, Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF;
	statement->if_.expression = expression;
	statement->if_.block = block;
	return statement;
}

Statement* ast_statement_if_else(Expression* exp, Block* if_, Block* else_) {
	assert(if_->tag == BLOCK && else_->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF_ELSE;
	statement->if_else.expression = exp;
	statement->if_else.if_block = if_;
	statement->if_else.else_block = else_;
	return statement;
}

Statement* ast_statement_while(Expression* expression, Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WHILE;
	statement->while_.expression = expression;
	statement->while_.block = block;
	return statement;
}

Statement* ast_statement_spawn(Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SPAWN;
	statement->spawn = block;
	return statement;
}

Statement* ast_statement_block(Block* block) {
	assert(block->tag == BLOCK);
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_BLOCK;
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
	variable->type = NULL;
	variable->value = false;
	variable->id = id;
	return variable;
}

Variable* ast_variable_indexed(Expression* array, Expression* index) {
	Variable* variable;
	MALLOC(variable, Variable);
	variable->tag = VARIABLE_INDEXED;
	variable->type = NULL;
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

Expression* ast_expression_literal_boolean(bool literal_boolean) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_BOOLEAN;
	expression->next = NULL;
	expression->type = NULL;
	expression->literal_boolean = literal_boolean;
	return expression;
}

Expression* ast_expression_literal_integer(int literal_integer) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_INTEGER;
	expression->next = NULL;
	expression->type = NULL;
	expression->literal_integer = literal_integer;
	return expression;
}

Expression* ast_expression_literal_float(double literal_float) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_FLOAT;
	expression->next = NULL;
	expression->type = NULL;
	expression->literal_float = literal_float;
	return expression;
}

Expression* ast_expression_literal_string(const char* literal_string) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_STRING;
	expression->next = NULL;
	expression->type = NULL;
	expression->literal_string = literal_string;
	return expression;
}

Expression* ast_expression_variable(Variable* variable) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_VARIABLE;
	expression->next = NULL;
	expression->type = NULL;
	expression->variable = variable;
	return expression;
}

Expression* ast_expression_function_call(FunctionCall* function_call) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_FUNCTION_CALL;
	expression->next = NULL;
	expression->type = NULL;
	expression->function_call = function_call;
	return expression;
}

Expression* ast_expression_unary(Token token, Expression* expression) {
	Expression* unaryExpression;
	MALLOC(unaryExpression, Expression);
	unaryExpression->tag = EXPRESSION_UNARY;
	expression->next = NULL;
	expression->type = NULL;
	unaryExpression->unary.token = token;
	unaryExpression->unary.expression = expression;
	return unaryExpression;
}

Expression* ast_expression_binary(Token token, Expression* l, Expression* r) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_BINARY;
	expression->next = NULL;
	expression->type = NULL;
	expression->binary.token = token;
	expression->binary.left_expression = l;
	expression->binary.right_expression = r;
	return expression;
}

Expression* ast_expression_cast(Expression* expression, Type* type) {
	assert(expression->type != type);

	Expression* castExpression;
	MALLOC(castExpression, Expression);
	castExpression->tag = EXPRESSION_CAST;

	// TODO: Test this with arguments in function calls (think it won't work)
	castExpression->next = expression->next;
	expression->next = NULL;

	// printf("cast type %s\n", type->id->name);
	castExpression->type = type;
	castExpression->cast = expression;
	return castExpression;
}

// ==================================================
//
//	Function Call
//
// ==================================================

FunctionCall* ast_function_call_basic(Id* id, Expression* arguments) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_BASIC;
	function_call->type = NULL;
	function_call->arguments = arguments;
	function_call->basic = id;
	return function_call;
}

FunctionCall* ast_function_call_method(Expression* o, Id* n, Expression* args) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_METHOD;
	function_call->type = NULL;
	function_call->arguments = args;
	function_call->method.object = o;
	function_call->method.name = n;
	return function_call;
}

FunctionCall* ast_function_call_constructor(Type* type, Expression* arguments) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
	function_call->type = NULL;
	function_call->arguments = arguments;
	function_call->constructor = type;
	return function_call;
}
