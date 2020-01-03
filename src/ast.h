#if !defined(ast_h)
#define ast_h

#include <stdbool.h>

#include <llvm-c/Core.h>

#include "scanner.h"

// ==================================================
//
//  Auxiliary
//
// ==================================================

typedef unsigned int Line; // TODO

typedef unsigned int Bitmap;

typedef enum FunctionQualifier {
    FQ_PRIVATE  = 1 << 0,
    FQ_ACQUIRE  = 1 << 1,
    FQ_RELEASE  = 1 << 2
} FunctionQualifier;

// ==================================================
//
//  LLVM
//
// ==================================================

typedef LLVMBuilderRef    LLVMB  ;
typedef LLVMModuleRef     LLVMM  ;
typedef LLVMContextRef    LLVMC  ;
typedef LLVMTypeRef       LLVMT  ;
typedef LLVMBasicBlockRef LLVMBB ;
typedef LLVMValueRef      LLVMV  ;

// ==================================================
//
//  Tags
//
// ==================================================

typedef enum DefinitionTag {
    DEFINITION_CAPSA,
    DECLARATION_FUNCTION, // TODO
    DEFINITION_FUNCTION,
    DEFINITION_METHOD,
    DEFINITION_CONSTRUCTOR,
    DEFINITION_TYPE
} DefinitionTag;

typedef enum TypeTag {
    TYPE_VOID,
    TYPE_ID,
    TYPE_ARRAY,
    TYPE_UNLOCKED,
    TYPE_INTERFACE,
    TYPE_STRUCTURE,
    TYPE_MONITOR
} TypeTag;

typedef enum BlockTag {
    BLOCK,
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
    STATEMENT_NUMERIC_FOR,
    STATEMENT_FOR,
    STATEMENT_SPAWN,
    STATEMENT_ACQUIRE_VALUE,
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
    EXPRESSION_LIST_COMPREHENSION,
    EXPRESSION_RANGE,
    EXPRESSION_CAST,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
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

// TODO: refactor
typedef Definition Def;
typedef Statement Stmt;
typedef Expression Exp;
typedef FunctionCall FC;

struct AST {
    Definition* definitions;
};

struct Definition {
    DefinitionTag tag;
    Definition* next;
    LLVMV V; // TODO: move this inside definition->function

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
            Bitmap qualifiers;
            Id* id;
            Definition* parameters;
            Type* type;
            Block* block;
            int vmti; // default -1
            Definition* pair; // acquire/release pair
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

    LLVMT T; // type

    union {
        // TypeID
        Id* id;
        // TypeArray
        Type* array;
        // TypeUnlocked
        Type* unlocked;
        // TypeInterface
        // TypeStructure
        // TypeMonitor
        struct {
            Id* id;
            Type* interface;
            Definition* definitions;
            // after semantic analysis
            Definition** attributes;
            size_t attributes_size;
            Definition** methods;
            size_t methods_size;
            Definition* constructor;
            // backend
            LLVMV gR, gL, gP; // VMTs
            LLVMT proxyT; // TODO: atc.c !!!
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
        // StatementNumericFor
        struct {
            Def* v;
            Exp* range;
            Block* block;
        } numeric_for;
        // StatementFor
        struct {
            Definition* initialization;
            Expression* condition;
            Statement* increment;
            Block* block;
        } for_;
        // StatementSpawn
        FunctionCall* spawn;
        // StatementAcquireValue
        struct {
            Definition* value; // scoped value
            Block* block;
        } acquire_value;
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
    LLVMV V; // default NULL
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
    LLVMV V;

    union {
        struct {
            bool immutable;
            union {
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
            };
        } literal;
        // ExpressionCapsa
        Capsa* capsa;
        // ExpressionFunctionCall
        FunctionCall* function_call;
        // ExpressionListComprehension
        struct {
            Expression* e;
            Definition* i;
            Expression* lower;
            Expression* upper;
            bool immutable; // default false
        } comprehension;
        // ExpressionRange
        struct {
            Token op;
            Expression* first;
            Expression* second;
            Expression* last;
        } range;
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

    Type* type;      // used by constructor calls before semantic analysis
    Expression* obj; // only used by methods (NULL for other types of calls)

    Id* id; // name of the function being called (NULL for constructors)

    Expression* arguments; // list of function arguments
    int argc;              // initialized with -1
    Definition* fn;        // function definition used in the backend module
};

// ==================================================
//
//  Functions & Capsa
//
// ==================================================

extern AST* ast;
extern void ast_set(Definition*);

extern Definition* ast_declaration_function(Id*, Definition*, Type*);

extern Definition* ast_definition_capsa(Capsa*, Expression*);
extern Definition* ast_definition_function(Definition*, Block*);
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
extern Type* ast_type_unlocked(Type*);
extern Type* ast_type_array(Type*);
extern Type* ast_type_structure(Id*, TypeTag, Definition*, Type*);

extern Block* ast_block(Line, Block*);
extern Block* ast_block_definition(Definition*);
extern Block* ast_block_statement(Statement*);

extern Stmt* ast_statement_assignment(Line, Token, Capsa*, Exp*);
extern Stmt* ast_statement_function_call(FC*);
extern Stmt* ast_statement_wait_for_in(Line, Exp*, Exp*);
extern Stmt* ast_statement_signal(Line, Exp*);
extern Stmt* ast_statement_broadcast(Line, Exp*);
extern Stmt* ast_statement_return(Line, Exp*);
extern Stmt* ast_statement_if(Line, Exp*, Block*);
extern Stmt* ast_statement_if_else(Line, Exp*, Block*, Block*);
extern Stmt* ast_statement_while(Line, Exp*, Block*);
extern Stmt* ast_stmt_numeric_for(Line, Id*, Exp*, Block*);
extern Stmt* ast_statement_for(Line, Def*, Exp*, Stmt*, Block*);
extern Stmt* ast_statement_spawn(Line, Block*);
extern Stmt* ast_statement_acquire_value(Line, Def*, Block*);
extern Stmt* ast_statement_block(Block*);

extern Capsa* ast_capsa_id(Id*);
extern Capsa* ast_capsa_attribute(Expression*, Id*);
extern Capsa* ast_capsa_indexed(Line, Expression*, Expression*);

extern Exp* ast_expression_literal_boolean(Line, bool);
extern Exp* ast_expression_literal_integer(Line, int);
extern Exp* ast_expression_literal_float(Line, double);
extern Exp* ast_expression_literal_string(Line, const char*);
extern Exp* ast_expression_literal_array(Line, Exp*, bool);
extern Exp* ast_expression_capsa(Capsa*);
extern Exp* ast_expression_function_call(FC*);
extern Exp* ast_expression_comprehension(Line, Exp*, Id*, Exp*, Exp*);
extern Exp* ast_expression_range(Line, Token, Exp*, Exp*, Exp*);
extern Exp* ast_expression_unary(Line, Token, Exp*);
extern Exp* ast_expression_binary(Line, Token, Exp*, Exp*);
extern Exp* ast_expression_cast(Line, Exp*, Type*);

extern FC* ast_call(Line, Id*, Exp*);
extern FC* ast_call_method(Line, Exp*, Id*, Exp*);
extern FC* ast_call_constructor(Line, Type*, Exp*);

#endif
