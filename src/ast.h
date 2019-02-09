#if !defined(ast_h)
#define ast_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "scanner.h"

typedef unsigned int Line; // TODO

// ==================================================
//
//  Tags
//
// ==================================================

typedef enum DefinitionTag {
    DEFINITION_CAPSA,
    DEFINITION_FUNCTION,
    DEFINITION_METHOD,
    DEFINITION_CONSTRUCTOR,
    DEFINITION_TYPE
} DefinitionTag;

typedef enum TypeTag {
    TYPE_VOID,
    TYPE_ID,
    TYPE_ARRAY,
    TYPE_INTERFACE,
    TYPE_STRUCTURE,
    TYPE_MONITOR
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
    STATEMENT_WAIT_FOR_IN,
    STATEMENT_SIGNAL,
    STATEMENT_BROADCAST,
    STATEMENT_RETURN,
    STATEMENT_IF,
    STATEMENT_IF_ELSE,
    STATEMENT_WHILE,
    STATEMENT_FOR,
    STATEMENT_SPAWN,
    STATEMENT_BLOCK
} StatementTag;

typedef enum CapsaTag {
    CAPSA_ID,
    CAPSA_ATTRIBUTE,
    CAPSA_INDEXED
} CapsaTag;

typedef enum ExpressionTag {
    EXPRESSION_LITERAL_BOOLEAN,
    EXPRESSION_LITERAL_INTEGER,
    EXPRESSION_LITERAL_FLOAT,
    EXPRESSION_LITERAL_STRING,
    EXPRESSION_LITERAL_ARRAY,
    EXPRESSION_CAPSA,
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
//  Nodes
//
// ==================================================

typedef struct AST AST;
typedef struct Definition Definition;
typedef struct Id Id;
typedef struct Type Type;
typedef struct Block Block;
typedef struct Statement Statement;
typedef struct Capsa Capsa;
typedef struct Expression Expression;
typedef struct FunctionCall FunctionCall;

struct AST {
    Definition* definitions;
};

struct Definition {
    DefinitionTag tag;
    Definition* next;
    LLVMValueRef llvm_value;
    
    union {
        // DefinitionCapsa
        struct {
            Capsa* capsa;
            Expression* expression;
        } capsa;
        // DefinitionFunction
        // DefinitionMethod
        // DefinitionConstructor
        struct {
            bool private;
            Id* id;
            Definition* parameters;
            Type* type;
            Block* block;
        } function;
        // DefinitionType
        Type* type;
    };
};

struct Id {
    Line line;
    const char* name;
};

struct Type {
    TypeTag tag;
    bool primitive;
    bool immutable; // default false

    LLVMTypeRef llvm_type; // TODO: always set

    union {
        // TypeID
        Id* id;
        // TypeArray
        Type* array;
        // TypeInterface
        // TypeStructure
        // TypeMonitor
        struct {
            Id* id;
            Type* interface;
            Definition* definitions;
            // after semantics analysis
            Definition** attributes;
            size_t attributes_size;
            Definition** methods;
            size_t methods_size;
            Definition* constructor;
        } structure;
    };
};

struct Block {
    BlockTag tag;
    Line line;
    Block* next;

    union {
        Definition* definition;
        Statement* statement;
    };
};

struct Statement {
    StatementTag tag;
    Line line;

    union {
        // StatementAssignment
        struct {
            Capsa* capsa;
            Expression* expression;
        } assignment;
        // StatementFunctionCall
        FunctionCall* function_call;
        // StatementWaitForIn
        struct {
            Expression* condition;
            Expression* queue;
        } wait_for_in;
        // StatementSignal
        Expression* signal;
        // StatementBroadcast
        Expression* broadcast;
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
        // StatementFor
        struct {
            Definition* initialization;
            Expression* condition;
            Statement* increment;
            Block* block;
        } for_;
        // StatementSpawn
        FunctionCall* spawn;
        // StatementBlock
        Block* block;
    };
};

struct Capsa {
    CapsaTag tag;
    Line line;

    Type* type; // default NULL
    bool global; // default false
    bool value; // default false
    LLVMValueRef llvm_value; // default NULL
    int llvm_structure_index; // default -1

    union {
        // CapsaId
        Id* id;
        // CapsaAttribute
        struct {
            Expression* structure;
            Id* field;
        } attribute;
        // CapsaIndexed
        struct {
            Expression* array;
            Expression* index;
        } indexed;
    };
};

struct Expression {
    ExpressionTag tag;
    Line line;
    Expression *previous, *next;
    Type* type;
    LLVMValueRef llvm_value;
    
    union {
        struct {
            bool immutable;
            // ExpressionLiteralBoolean
            bool boolean;
            // ExpressionLiteralInteger
            int integer;
            // ExpressionLiteralFloat
            double float_;
            // ExpressionLiteralString
            const char* string;
            // ExpressionLiteralArray
            Expression* array;
        } literal;
        // ExpressionCapsa
        Capsa* capsa;
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
    Line line;

    // Used by constructor calls before semantic analysis
    Type* type;
    // Only used by methods (NULL for other types of calls)
    Expression* instance;
    // Name of the function being called (NULL for constructors)
    Id* id;
    Expression* arguments;
    // Initialized with -1
    int argument_count;

    // Used in the backend module
    Definition* function_definition;
};

// ==================================================
//
//  Functions & Capsa
//
// ==================================================

extern AST* ast;
extern void ast_set(Definition*);

extern Definition* ast_definition_capsa(Capsa*, Expression*);
extern Definition* ast_definition_function(Id*, Definition*, Type*, Block*);
extern Definition* ast_definition_method(bool, Definition*);
extern Definition* ast_definition_constructor(Definition*, Block*);
extern Definition* ast_definition_type(Type*);

extern Id* ast_id(Line, const char*);

extern Type* ast_type_void(void);
extern Type* ast_type_boolean(void);
extern Type* ast_type_integer(void);
extern Type* ast_type_float(void);
extern Type* ast_type_string(void);
extern Type* ast_type_condition_queue(void);
extern Type* ast_type_id(Id*);
extern Type* ast_type_array(Type*);
extern Type* ast_type_structure(Id*, TypeTag, Definition*, Type*);

extern Block* ast_block(Line, Block*);
extern Block* ast_block_definition(Definition*);
extern Block* ast_block_statement(Statement*);

extern Statement* ast_statement_assignment(Line, Token, Capsa*, Expression*);
extern Statement* ast_statement_function_call(FunctionCall*);
extern Statement* ast_statement_wait_for_in(Line, Expression*, Expression*);
extern Statement* ast_statement_signal(Line, Expression*);
extern Statement* ast_statement_broadcast(Line, Expression*);
extern Statement* ast_statement_return(Line, Expression*);
extern Statement* ast_statement_if(Line, Expression*, Block*);
extern Statement* ast_statement_if_else(Line, Expression*, Block*, Block*);
extern Statement* ast_statement_while(Line, Expression*, Block*);
extern Statement* ast_statement_for(Line, Definition*, Expression*, Statement*,
    Block*);
extern Statement* ast_statement_spawn(Line, Block*);
extern Statement* ast_statement_block(Block*);

extern Capsa* ast_capsa_id(Id*);
extern Capsa* ast_capsa_attribute(Expression*, Id*);
extern Capsa* ast_capsa_indexed(Line, Expression*, Expression*);

extern Expression* ast_expression_literal_boolean(Line, bool);
extern Expression* ast_expression_literal_integer(Line, int);
extern Expression* ast_expression_literal_float(Line, double);
extern Expression* ast_expression_literal_string(Line, const char*);
extern Expression* ast_expression_literal_array(Line, Expression*, bool);
extern Expression* ast_expression_capsa(Capsa*);
extern Expression* ast_expression_function_call(FunctionCall*);
extern Expression* ast_expression_unary(Line, Token, Expression*);
extern Expression* ast_expression_binary(Line, Token, Expression*, Expression*);
extern Expression* ast_expression_cast(Expression*, Type*);

extern FunctionCall* ast_call(Line, Id*, Expression*);
extern FunctionCall* ast_call_method(Line, Expression*, Id*, Expression*);
extern FunctionCall* ast_call_constructor(Line, Type*, Expression*);

#endif
