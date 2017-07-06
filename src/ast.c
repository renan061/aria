#include <assert.h>
#include <string.h>
#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "alloc.h"
#include "scanner.h" // for native_types

// ==================================================
//
//	AST
//
// ==================================================

AST* ast = NULL;

void ast_set(Definition* definitions) {
	MALLOC(ast, AST);
	ast->definitions = definitions;
}

// ==================================================
//
//	Definition
//
// ==================================================

Definition* ast_definition_variable(Variable* var, Expression* exp) {
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_VARIABLE;
	definition->next = NULL;
	definition->llvm_value = NULL;
	definition->variable.variable = var;
	definition->variable.expression = exp;
	return definition;
}

// Used by function and constructor definitions
#define AST_FUNCTION(f, _tag, _private, _id, _parameters, _type, _block) \
	assert(_block->tag == BLOCK); \
	f->tag = _tag; \
	f->next = NULL; \
	f->llvm_value = NULL; \
	f->function.private = _private; \
	f->function.id = _id; \
	f->function.parameters = _parameters; \
	f->function.type = _type; \
	f->function.block = _block; \

Definition* ast_definition_function(Id* id, Definition* ps, Type* t, Block* b) {
	Definition* definition;
	MALLOC(definition, Definition);
	AST_FUNCTION(definition, DEFINITION_FUNCTION,
		/* Private		*/ false,
		/* Id			*/ id,
		/* Parameters	*/ ps,
		/* Type			*/ t,
		/* Block		*/ b
	);
	return definition;
}

Definition* ast_definition_method(bool private, Definition* function) {
	assert(function->tag == DEFINITION_FUNCTION);
	function->tag = DEFINITION_METHOD;
	function->function.private = private;
	return function;
}

Definition* ast_definition_constructor(Definition* parameters, Block* block) {
	Definition* definition;
	MALLOC(definition, Definition);
	AST_FUNCTION(definition, DEFINITION_CONSTRUCTOR,
		/* Private		*/ false,
		/* Id			*/ NULL,
		/* Parameters	*/ parameters,
		/* Type			*/ NULL,
		/* Block		*/ block
	);
	return definition;
}

Definition* ast_definition_type(Type* type) {
	assert(type->tag == TYPE_ID || type->tag == TYPE_MONITOR);
	Definition* definition;
	MALLOC(definition, Definition);
	definition->tag = DEFINITION_TYPE;
	definition->next = NULL;
	definition->llvm_value = NULL;
	definition->type = type;
	return definition;
}

// ==================================================
//
//	Id
//
// ==================================================

Id* ast_id(Line line, const char* name) {
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
#define NATIVE_TYPE(v, i)						\
	static Type* v = NULL;						\
	if (!v) {									\
		MALLOC(v, Type);						\
		v->tag = TYPE_ID;						\
		v->primitive = true;					\
		v->immutable = true;					\
		v->llvm_type = NULL;					\
		v->id = ast_id(-1, native_types[i]);	\
	}											\
	return v;									\

Type* ast_type_void(void) {
	static Type* type_void = NULL;
	if (!type_void) {
		MALLOC(type_void, Type);
		type_void->tag = TYPE_VOID;
		type_void->primitive = true;
		type_void->immutable = true;
		type_void->llvm_type = NULL;
		type_void->id = NULL; // just in case
	}
	return type_void;
}

Type* ast_type_boolean(void) {
	NATIVE_TYPE(type_boolean, SCANNER_NATIVE_BOOLEAN);
}

Type* ast_type_integer(void) {
	NATIVE_TYPE(type_integer, SCANNER_NATIVE_INTEGER);
}

Type* ast_type_float(void) {
	NATIVE_TYPE(type_float, SCANNER_NATIVE_FLOAT);
}

Type* ast_type_string(void) {
	NATIVE_TYPE(type_string, SCANNER_NATIVE_STRING);
}

Type* ast_type_condition_queue(void) {
	// TODO: Refactor
	static Type* type_condition_queue = NULL;
	if (!type_condition_queue) {
		MALLOC(type_condition_queue, Type);
		type_condition_queue->tag = TYPE_ID;
		type_condition_queue->primitive = true;
		type_condition_queue->immutable = false;
		type_condition_queue->llvm_type = NULL;
		type_condition_queue->id = ast_id(
			-1, native_types[SCANNER_NATIVE_CONDITION_QUEUE]
		);
	}
	return type_condition_queue;
}

static Type* checknative(Id* id) {
	#define CHECK_TYPE(i, v)					\
		if (native_types[i] == id->name) {		\
			return free(id), v;					\
		}										\

	CHECK_TYPE(SCANNER_NATIVE_BOOLEAN, ast_type_boolean());
	CHECK_TYPE(SCANNER_NATIVE_INTEGER, ast_type_integer());
	CHECK_TYPE(SCANNER_NATIVE_FLOAT, ast_type_float());
	CHECK_TYPE(SCANNER_NATIVE_STRING, ast_type_string());
	CHECK_TYPE(SCANNER_NATIVE_CONDITION_QUEUE, ast_type_condition_queue());
	return NULL;
}

Type* ast_type_id(Id* id) {
	Type* type;
	if ((type = checknative(id))) {
		return type;
	}

	MALLOC(type, Type);
	type->tag = TYPE_ID;
	type->primitive = false;
	type->immutable = false;
	type->llvm_type = NULL;
	type->id = id;
	return type;
}

Type* ast_type_array(Type* type) {
	Type* arrayType;
	MALLOC(arrayType, Type);
	arrayType->tag = TYPE_ARRAY;
	arrayType->primitive = false;
	arrayType->immutable = false;
	arrayType->llvm_type = NULL;
	arrayType->array = type;
	return arrayType;
}

Type* ast_type_monitor(Id* id, Definition* definitions) {
	Type* type;
	MALLOC(type, Type);
	type->tag = TYPE_MONITOR;
	type->primitive = false;
	type->immutable = false;
	type->llvm_type = NULL;
	type->monitor.id = id;
	type->monitor.definitions = definitions;
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

Statement* ast_statement_wait_for_in(Line ln, Expression* c, Expression* q) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_WAIT_FOR_IN;
	statement->line = ln;
	statement->wait_for_in.condition = c;
	statement->wait_for_in.queue = q;
	return statement;
}

Statement* ast_statement_signal(Line ln, Expression* expression) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_SIGNAL;
	statement->line = ln;
	statement->signal = expression;
	return statement;
}

Statement* ast_statement_broadcast(Line ln, Expression* expression) {
	Statement* statement;
	MALLOC(statement, Statement);
	statement->tag = STATEMENT_BROADCAST;
	statement->line = ln;
	statement->broadcast = expression;
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
	statement->spawn = ast_call(ln, /* id */ NULL, /* arguments */ NULL);
	statement->spawn->function_definition = ast_definition_function(
		/* id */ NULL, /* parameters */ NULL, ast_type_void(), block
	);
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
	variable->llvm_value = NULL;
	variable->llvm_structure_index = -1;
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
	variable->llvm_value = NULL;
	variable->llvm_structure_index = -1;
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
	expression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	unaryExpression->llvm_value = NULL;
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
	expression->llvm_value = NULL;
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
	castExpression->llvm_value = NULL;
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
	function_call->instance = NULL;
	function_call->id = id;
	function_call->arguments = arguments;
	function_call->argument_count = -1;
	function_call->function_definition = NULL;
	return function_call;
}

FunctionCall* ast_call_method(Line ln, Expression* i, Id* id, Expression* a) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_METHOD;
	function_call->line = ln;
	function_call->type = NULL;
	function_call->instance = i;
	function_call->id = id;
	function_call->arguments = a;
	function_call->argument_count = -1;
	function_call->function_definition = NULL;
	return function_call;
}

FunctionCall* ast_call_constructor(Line ln, Type* type, Expression* arguments) {
	FunctionCall* function_call;
	MALLOC(function_call, FunctionCall);
	function_call->tag = FUNCTION_CALL_CONSTRUCTOR;
	function_call->line = ln;
	function_call->type = type;
	function_call->instance = NULL;
	function_call->id = NULL;
	function_call->arguments = arguments;
	function_call->argument_count = -1;
	function_call->function_definition = NULL;
	return function_call;
}
