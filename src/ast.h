#if !defined(ast_h)
#define ast_h

#include <stdbool.h>

#include "scanner.h"

// ==================================================
//
//	Tags
//
// ==================================================

typedef enum BodyTag {
	BODY,
	BODY_DECLARATION,
	BODY_DEFINITION
} BodyTag;

typedef enum DeclarationTag {
	DECLARATION_VARIABLE,
	DECLARATION_FUNCTION
} DeclarationTag;

typedef enum DefinitionTag {
	DEFINITION_VARIABLE,
	DEFINITION_FUNCTION,
	DEFINITION_METHOD,
	DEFINITION_CONSTRUCTOR,
	DEFINITION_MONITOR,
} DefinitionTag;

typedef enum TypeTag {
	// TYPE_VOID, TYPE_NIL TODO
	TYPE_ID,
	TYPE_MONITOR, // TODO
	TYPE_ARRAY,
} TypeTag;

typedef enum BlockTag {
	BLOCK,
	BLOCK_DECLARATION,
	BLOCK_DEFINITION,
	BLOCK_STATEMENT
} BlockTag;

typedef enum StatementTag {
	STATEMENT_ASSIGNMENT,
	STATEMENT_FUNCTION_CALL,
	STATEMENT_WHILE_WAIT,
	STATEMENT_SIGNAL,
	STATEMENT_BROADCAST,
	STATEMENT_RETURN,
	STATEMENT_IF,
	STATEMENT_IF_ELSE,
	STATEMENT_WHILE,
	STATEMENT_SPAWN,
	STATEMENT_BLOCK
} StatementTag;

typedef enum VariableTag {
	VARIABLE_ID,
	VARIABLE_INDEXED
} VariableTag;

typedef enum ExpressionTag {
	EXPRESSION_LITERAL_BOOLEAN,
	EXPRESSION_LITERAL_INTEGER,
	EXPRESSION_LITERAL_FLOAT,
	EXPRESSION_LITERAL_STRING,
	EXPRESSION_VARIABLE,
	EXPRESSION_FUNCTION_CALL,
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST
} ExpressionTag;

typedef enum FunctionCallTag {
	FUNCTION_CALL_BASIC,
	FUNCTION_CALL_METHOD,
	FUNCTION_CALL_CONSTRUCTOR
} FunctionCallTag;

// ==================================================
//
//	Nodes
//
// ==================================================

typedef struct Program Program;
typedef struct Body Body;
typedef struct Declaration Declaration;
typedef struct Definition Definition;
typedef struct Id Id;
typedef struct Type Type;
typedef struct Block Block;
typedef struct Statement Statement;
typedef struct Variable Variable;
typedef struct Expression Expression;
typedef struct FunctionCall FunctionCall;

// TODO: Remove program and stay with body only?
struct Program {
	Body* body;
};

struct Body {
	BodyTag tag;
	Body* next;

	union {
		Declaration* declaration;
		Definition* definition;
	};
};

struct Declaration {
	DeclarationTag tag;
	Declaration* next; // parameters

	union {
		// DeclarationVariable
		Variable* variable;
		// DeclarationFunction
		struct {
			Id* id;
			Declaration* parameters;
			Type* type;
		} function;
	};
};

struct Definition {
	DefinitionTag tag;
	
	union {
		// DefinitionVariable
		struct {
			Declaration* declaration;
			Expression* expression;
		} variable;
		// DefinitionFunction
		// DefinitionConstructor
		struct {
			Declaration* declaration;
			Block* block;
		} function;
		// DefinitionMethod
		struct {
			Definition* function;
			bool private;
		} method;
		// DefinitionMonitor
		struct {
			Id* id;
			Body* body;
		} monitor;
	};
};

struct Id {
	unsigned int line;

	union {
		// Name
		const char* name;
		// Declaration
		Declaration* declaration;
	};
};

struct Type {
	TypeTag tag;
	bool primitive;

	union {
		// TypeID
		Id* id;
		// TypeMonitor
		Definition* monitor; // TODO
		// TypeArray
		Type* array;
	};
};

struct Block {
	BlockTag tag;
	Block* next;

	union {
		Declaration* declaration;
		Definition* definition;
		Statement* statement;
	};
};

struct Statement {
	StatementTag tag;

	union {
		// StatementAssignment
		struct {
			Variable* variable;
			Expression* expression;
		} assignment;
		// StatementFunctionCall
		FunctionCall* function_call;
		// StatementWhileWait
		struct {
			Expression* expression;
			Variable* variable;
		} while_wait;
		// StatementSignal
		Variable* signal;
		// StatementBroadcast
		Variable* broadcast;
		// StatementReturn
		Expression* return_;
		// StatementIf
		struct {
			Expression* expression;
			Block* block;
		} if_;
		// StatementIfElse
		struct {
			Expression* expression;
			Block* if_block;
			Block* else_block;
		} if_else;
		// StatementWhile
		struct {
			Expression* expression;
			Block* block;
		} while_;
		// StatementSpawn
		Block* spawn;
		// StatementBlock
		Block* block;
	};
};

struct Variable {
	VariableTag tag;
	Type* type; // redundant
	bool value; // default false

	union {
		// VariableId
		Id* id;
		// VariableIndexed
		struct {
			Expression* array;
			Expression* index;
		} indexed;
	};
};

struct Expression {
	ExpressionTag tag;
	Expression* next;
	Type* type;
	
	union {
		// ExpressionLiteralBoolean
		bool literal_boolean;
		// ExpressionLiteralInteger
		int literal_integer;
		// ExpressionLiteralFloat
		double literal_float;
		// ExpressionLiteralString
		const char* literal_string;
		// ExpressionVariable
		Variable* variable;
		// ExpressionFunctionCall
		FunctionCall* function_call;
		// ExpressionUnary
		struct {
			Token token;
			Expression* expression;
		} unary;
		// ExpressionBinary
		struct {
			Token token;
			Expression* left_expression;
			Expression* right_expression;
		} binary;
		// ExpressionCast
		Expression* cast;
	};
};

struct FunctionCall {
	FunctionCallTag tag;
	Type* type; // redundant
	Expression* arguments;

	union {
		// FunctionCallBasic
		Id* basic;
		// FunctionCallMethod
		struct {
			Expression* object; // TODO: Rename to instance?
			Id* name; // TODO: Rename to id?
		} method;
		// FunctionCallConstructor
		Type* constructor;
	};
};

// ==================================================
//
//	Functions & Variable
//
// ==================================================

extern Program* program;
extern Program* ast_program(Body*);

extern Body* ast_body(Body*);
extern Body* ast_body_declaration(Declaration*);
extern Body* ast_body_definition(Definition*);

extern Declaration* ast_declaration_variable(Variable*);
extern Declaration* ast_declaration_function(Id*, Declaration*, Type*);

extern Definition* ast_definition_variable(Declaration*, Expression*);
extern Definition* ast_definition_function(Declaration*, Block*);
extern Definition* ast_definition_method(Definition*, bool);
extern Definition* ast_definition_constructor(Declaration*, Block*);
extern Definition* ast_definition_monitor(Id*, Body*);

extern Id* ast_id(unsigned int, const char*);

extern Type* ast_type_boolean(void);
extern Type* ast_type_integer(void);
extern Type* ast_type_float(void);
extern Type* ast_type_string(void);
extern Type* ast_type_id(Id*);
extern Type* ast_type_array(Type*);

extern Block* ast_block(Block*);
extern Block* ast_block_declaration(Declaration*);
extern Block* ast_block_definition(Definition*);
extern Block* ast_block_statement(Statement*);

extern Statement* ast_statement_assignment(Variable*, Expression*);
extern Statement* ast_statement_function_call(FunctionCall*);
extern Statement* ast_statement_while_wait(Expression*, Variable*);
extern Statement* ast_statement_signal(Variable*);
extern Statement* ast_statement_broadcast(Variable*);
extern Statement* ast_statement_return(Expression*);
extern Statement* ast_statement_if(Expression*, Block*);
extern Statement* ast_statement_if_else(Expression*, Block*, Block*);
extern Statement* ast_statement_while(Expression*, Block*);
extern Statement* ast_statement_spawn(Block*);
extern Statement* ast_statement_block(Block*);

extern Variable* ast_variable_id(Id*);
extern Variable* ast_variable_indexed(Expression*, Expression*);

extern Expression* ast_expression_literal_boolean(bool);
extern Expression* ast_expression_literal_integer(int);
extern Expression* ast_expression_literal_float(double);
extern Expression* ast_expression_literal_string(const char*);
extern Expression* ast_expression_variable(Variable*);
extern Expression* ast_expression_function_call(FunctionCall*);
extern Expression* ast_expression_unary(Token, Expression*);
extern Expression* ast_expression_binary(Token, Expression*, Expression*);
extern Expression* ast_expression_cast(Expression*, Type*);

extern FunctionCall* ast_function_call_basic(Id*, Expression*);
extern FunctionCall* ast_function_call_method(Expression*, Id*, Expression*);
extern FunctionCall* ast_function_call_constructor(Type*, Expression*);

#endif
