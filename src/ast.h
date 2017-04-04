#if !defined(ast_h)
#define ast_h

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
	TYPE_INDEXED,
} TypeTag;

typedef enum StatementTag {
	// simple
	STATEMENT_ASSIGNMENT,
	STATEMENT_DEFINITION,
	STATEMENT_FUNCTION_CALL,
	STATEMENT_WHILE_WAIT,
	STATEMENT_SIGNAL,
	STATEMENT_BROADCAST,
	STATEMENT_RETURN,
	// compound
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
	EXPRESSION_BINARY,
	EXPRESSION_UNARY,
	// primary
	EXPRESSION_LITERAL_BOOLEAN,
	EXPRESSION_LITERAL_INTEGER,
	EXPRESSION_LITERAL_FLOAT,
	EXPRESSION_LITERAL_STRING,
	EXPRESSION_VARIABLE,
	EXPRESSION_FUNCTION_CALL
} ExpressionTag;

// TODO: Monitor

// ==================================================
//
//	Nodes
//
// ==================================================

typedef struct ProgramNode		ProgramNode;
typedef struct DefinitionNode	DefinitionNode;
typedef struct TypeNode			TypeNode;
typedef struct IdNode			IdNode;
typedef struct StatementNode	StatementNode;
typedef struct VariableNode		VariableNode;
typedef struct ExpressionNode	ExpressionNode;
typedef struct FunctionCallNode	FunctionCallNode;

struct ProgramNode {
	DefinitionNode* defs;
};

struct DefinitionNode {
	DefinitionTag tag;
	DefinitionNode* next;
	
	union {
		// DefinitionFunction
		struct {
			TypeNode* type;
			IdNode* id;
			bool global;
			unsigned int temp;
		} function;
		// DefinitionMonitor
		struct {
			// TODO
		} monitor;
	};
};

struct TypeNode {
	TypeE tag;
	union {
		// TypeID
		IdNode* id;
		// TypeIndexed
		TypeNode* indexed;
	};
};

struct IdNode {
	int line;
	union {
		// String
		const char* string;
		// Declaration
		// TODO (DefinitionNode* definition;)
	};
};

struct StatementNode {
	StatementTag tag;
	StatementNode* next;

	int line;

	union {
		// StatementAssignment

		// StatementDefinition
		struct {
			VariableNode* variable;
			ExpressionNode* expression;
		};
		// StatementFunctionCall
		STATEMENT_FUNCTION_CALL,
		// StatementWhileWait
		STATEMENT_WHILE_WAIT,
		// StatementSignal, StatementBroadcast
		STATEMENT_SIGNAL,
		STATEMENT_BROADCAST,
		// StatementReturn
		STATEMENT_RETURN,
		// StatementIf
		STATEMENT_IF,
		// StatementIfElse
		STATEMENT_IF_ELSE,
		// StatementWhile
		STATEMENT_WHILE,
		// StatementSpawn
		STATEMENT_SPAWN,
		// StatementBlock
		STATEMENT_BLOCK
	};
}




























struct StatementNode {
	CmdE tag;
	int line;
	StatementNode* next;

	union {
		// CmdBlock
		struct {
			DefinitionNode* defs;
			StatementNode* cmds;
		} block;
		// CmdIf and CmdWhile
		struct {
			ExpressionNode* exp;
			StatementNode* cmd;
		} ifwhile;
		// CmdIfElse
		struct {
			ExpressionNode* exp;
			StatementNode *ifcmd, *elsecmd;
		} ifelse;
		// CmdPrint
		ExpressionNode* print;
		// CmdAsg
		struct {
			VariableNode* var;
			ExpressionNode* exp;
		} asg;
		// CmdReturn
		ExpressionNode* ret;
		// CmdCall
		FunctionCallNode* call;
	} u;
};

struct VariableNode {
	VarE tag;
	int line;
	unsigned int temp;
	TypeNode* type;

	union {
		// VarId
		IdNode* id;
		// VarIndexed
		struct {
			ExpressionNode *array, *index;
		} indexed;
	} u;
};

struct ExpressionNode {
	ExpE tag;
	int line; // Initialized as "-1" for ExpInt, ExpFloat and ExpStr
	unsigned int temp;
	TypeNode* type;
	ExpressionNode* next;
	
	union {
		// ExpKInt
		int intvalue;
		// ExpKFloat
		double floatvalue;
		// ExpKStr
		const char* strvalue;
		// ExpVar
		VariableNode* var;
		// ExpCall
		FunctionCallNode* call;
		// ExpNew
		struct {
			TypeNode* type;
			ExpressionNode* size;
		} new;
		// ExpCast
		ExpressionNode* cast;
		// ExpUnary
		struct {
			ScannerSymbol symbol;
			ExpressionNode* exp;
		} unary;
		// ExpMul, ExpAdd, ExpComp, ExpAnd and ExpOr
		struct {
			ScannerSymbol symbol;
			ExpressionNode *exp1, *exp2;
		} binary;
	} u;
};

struct FunctionCallNode {
	IdNode* id;
	ExpressionNode* args;
};

#endif
