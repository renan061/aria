#include "ast.h"
#include "alloc.h"

// ==================================================
//
//	Program
//
// ==================================================

Program* program;

Program* ast_program(Body* body) {
	Program* program;
	MALLOC(program, Program);
	program->body = body;
	return program;
}

// ==================================================
//
//	Declaration
//
// ==================================================

Declaration* ast_declaration_variable(Id* id, Type* type) {
	Declaration* declaration;
	MALLOC(declaration, Declaration);
	declaration->next = NULL;
	declaration->id = id;
	declaration->type = type;
	return declaration;
}

// ==================================================
//
//	Definition
//
// ==================================================

Definition* ast_definition_function(Id* id, Declaration* p, Type* t, Block* b) {
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_FUNCTION;
	definition->function.id = id;
	definition->function.parameters = p;
	definition->function.type = t;
	definition->function.block = b;
	return definition;
}

Definition* ast_definition_method(bool private, Definition* function) {
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_METHOD;
	definition->method.private = private;
	definition->method.function = function;
	return definition;
}

Definition* ast_definition_constructor(Declaration* parameters, Block* block) {
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_CONSTRUCTOR;
	definition->constructor.parameters = parameters;
	definition->constructor.block = block;
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

Id* ast_id(const char* name) {
	Id* id;
	MALLOC(id, Id);
	id->name = name;
	return id;
}

// ==================================================
//
//	Type
//
// ==================================================

Type* ast_type_id(Id* id) {
	Type* type;
	MALLOC(type, Type);
	type->tag = TYPE_ID;
	type->id = id;
	return type;
}

Type* ast_type_array(Type* type) {
	Type* arrayType;
	MALLOC(arrayType, Type);
	arrayType->tag = TYPE_ARRAY;
	arrayType->array = type;
	return arrayType;	
}

// ==================================================
//
//	Block
//
// ==================================================

Block* ast_block_declaration(Declaration* declaration) {
	Block* block;
	MALLOC(block, Block);
	block->tag = BLOCK_DECLARATION;
	block->next = NULL;
	block->declaration = declaration;
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
//	Body
//
// ==================================================

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

Statement* ast_statement_definition(Id* id, Expression* expression) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_DEFINITION;
	statement->definition.declaration = ast_declaration_variable(id, NULL);
	statement->definition.expression = expression;
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
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF;
	statement->if_.expression = expression;
	statement->if_.block = block;
	return statement;
}

Statement* ast_statement_if_else(Expression* exp, Block* if_, Block* else_) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_IF_ELSE;
	statement->if_else.expression = exp;
	statement->if_else.if_block = if_;
	statement->if_else.else_block = else_;
	return statement;
}

Statement* ast_statement_while(Expression* expression, Block* block) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WHILE;
	statement->while_.expression = expression;
	statement->while_.block = block;
	return statement;
}

Statement* ast_statement_spawn(Block* block) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SPAWN;
	statement->spawn = block;
	return statement;
}

Statement* ast_statement_block(Block* block) {
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
	variable->id = id;
	return variable;
}

Variable* ast_variable_indexed(Expression* array, Expression* index) {
	Variable* variable;
	MALLOC(variable, Variable);
	variable->tag = VARIABLE_INDEXED;
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
	expression->literal_boolean = literal_boolean;
	return expression;
}

Expression* ast_expression_literal_integer(int literal_integer) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_INTEGER;
	expression->next = NULL;
	expression->literal_integer = literal_integer;
	return expression;
}

Expression* ast_expression_literal_float(double literal_float) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_FLOAT;
	expression->next = NULL;
	expression->literal_float = literal_float;
	return expression;
}

Expression* ast_expression_literal_string(const char* literal_string) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_LITERAL_STRING;
	expression->next = NULL;
	expression->literal_string = literal_string;
	return expression;
}

Expression* ast_expression_variable(Variable* variable) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_VARIABLE;
	expression->next = NULL;
	expression->variable = variable;
	return expression;
}

Expression* ast_expression_function_call(FunctionCall* function_call) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_FUNCTION_CALL;
	expression->next = NULL;
	expression->function_call = function_call;
	return expression;
}

Expression* ast_expression_unary(Token token, Expression* expression) {
	Expression* unaryExpression;
	MALLOC(unaryExpression, Expression);
	unaryExpression->tag = EXPRESSION_UNARY;
	expression->next = NULL;
	unaryExpression->unary.token = token;
	unaryExpression->unary.expression = expression;
	return unaryExpression;
}

Expression* ast_expression_binary(Token token, Expression* l, Expression* r) {
	Expression* expression;
	MALLOC(expression, Expression);
	expression->tag = EXPRESSION_BINARY;
	expression->next = NULL;
	expression->binary.token = token;
	expression->binary.left_expression = l;
	expression->binary.right_expression = r;
	return expression;
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
	function_call->arguments = arguments;
	function_call->basic = id;
	return function_call;
}

FunctionCall* ast_function_call_method(Expression* o, Id* n, Expression* args) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_METHOD;
	function_call->arguments = args;
	function_call->method.object = o;
	function_call->method.name = n;
	return function_call;
}

FunctionCall* ast_function_call_constructor(Type* type, Expression* arguments) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
	function_call->arguments = arguments;
	function_call->constructor = type;
	return function_call;
}
