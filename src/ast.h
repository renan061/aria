#if !defined(ast_h)
#define ast_h

/*
 * TODO: Can I group up variable declarations and statements
 * together? Are variable declarations statements? (no)
 * Should I use some intermediary, like "content"?
 */


#include <stdbool.h>

// ==================================================
//
//	Tags
//
// ==================================================

typedef enum DefinitionTag {
	DEFINITION_FUNCTION,
	DEFINITION_MONITOR,
} DefinitionTag;

typedef enum TypeTag {
	TYPE_ID,
	TYPE_ARRAY,
} TypeTag;

typedef enum StatementTag {
	STATEMENT_ASSIGNMENT,
	STATEMENT_DEFINITION,
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
	EXPRESSION_BINARY
} ExpressionTag;

// ==================================================
//
//	Nodes
//
// ==================================================

typedef struct ProgramNode				ProgramNode;
typedef struct DefinitionNode			DefinitionNode;
typedef struct DeclarationNode			DeclarationNode;
typedef struct FunctionDefinitionNode	FunctionDefinitionNode;
typedef struct ClassDefinitionNode		ClassDefinitionNode;
typedef struct TypeNode					TypeNode;
typedef struct IdNode					IdNode;
typedef struct StatementNode			StatementNode;
typedef struct VariableNode				VariableNode;
typedef struct ExpressionNode			ExpressionNode;
typedef struct FunctionCallNode			FunctionCallNode;

struct ProgramNode {
	DefinitionNode* definitions;
};

struct DefinitionNode {
	DefinitionTag tag;
	
	union {
		// DefinitionFunction
		FunctionDefinitionNode* function;
		// DefinitionMonitor
		ClassDefinitionNode* monitor;
	};
};

struct DeclarationNode {
	IdNode* id;
	TypeNode* type;
};

struct FunctionDefinitionNode {
	IdNode* id;
	DeclarationNode* parameters;
	TypeNode* type;
	StatementNode* block;
};

struct ClassDefinitionNode {
	IdNode* id;
	ClassBodyContentNode* contents; // TODO: Declarations?	
}

struct ClassBodyContentNode {
	ClassBodyContentTag tag;

	union {
		// method
		// initializer
		// attribute
	};
}

struct TypeNode {
	TypeTag tag;

	union {
		// TypeID
		IdNode* id;
		// TypeArray
		TypeNode* array;
	};
};

struct IdNode {
	IdTag tag;

	union {
		// String
		const char* string;
		// Declaration
		DeclarationNode* declaration;
	};
};

struct StatementNode {
	StatementTag tag;

	union {
		// StatementAssignment
		struct {
			VariableNode* variable;
			ExpressionNode* expression;
		} assignment;
		// StatementDefinition
		struct {
			DeclarationNode* declaration;
			ExpressionNode* expression;
		} definition;
		// StatementFunctionCall
		FunctionCallNode* function_call;
		// StatementWhileWait
		struct {
			ExpressionNode* expression;
			VariableNode* variable;
		} while_wait;
		// StatementSignal
		VariableNode* signal;
		// StatementBroadcast
		VariableNode* broadcast;
		// StatementReturn
		ExpressionNode* return;
		// StatementIf
		struct {
			ExpressionNode* expression;
			StatementNode* block;
		} if;
		// StatementIfElse
		struct {
			ExpressionNode* expression;
			StatementNode* if_block;
			StatementNode* else_block;
		} if_else;
		// StatementWhile
		struct {
			ExpressionNode* expression;
			StatementNode* block;
		} while;
		// StatementSpawn
		StatementNode* spawn;
		// StatementBlock
		StatementBlock* block;
	};
};

struct VariableNode {
	VariableTag tag;

	union {
		// VarId
		IdNode* id;
		// VarIndexed
		struct {
			ExpressionNode* array;
			ExpressionNode* index;
		} indexed;
	};
};

struct ExpressionNode {
	ExpressionTag tag;
	
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
		VariableNode* variable;
		// ExpressionFunctionCall
		FunctionCallNode* function_call;
		// ExpressionUnary
		struct {
			char token; // TODO: Type
			ExpressionNode* expression;
		} unary;
		// ExpressionBinary
		struct {
			char token; // TODO: Type
			ExpressionNode* left_expression;
			ExpressionNode* right_expression;
		} binary;
	};
};

struct FunctionCallNode {
	FunctionCallTag* tag;
	ExpressionNode* arguments;

	union {
		// FunctionCallBasic
		IdNode* basic;
		// TODO1: When not array type can I call initializer?
		// That way would be a method call like m.initializer(...)
		// TODO2: What does Swift array implies? Ex.: [Integer] -> Array<Integer> 
		// FunctionCallConstructor
		TypeNode* constructor;
		// FunctionCallMethod
		struct {
			ExpressionNode* object;
			IdNode* name;
		} method;
	};
};

#endif
