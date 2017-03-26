#if !defined(scanner_h)
#define scanner_h

/*
 * Set yyin to change the scanner's input. If let
 * untouched, stdin will be used instead.
 */
extern FILE* yyin;

/*
 * Used by the parser to get the next token.
 */
extern int yylex(void);

/*
 * Setup and Clean should be called before starting
 * to use the scanner and after finishing doing so,
 * respectively.
 */
extern void scanner_setup(void);
extern void scanner_clean(void);

// TODO: Temporary
typedef struct SemInfo {
	unsigned int line;
	union {
		int ival;
		double fval;
		const char* strval; // TODO: Really constant?
	} u;
} SemInfo;

SemInfo yylval;

// TODO: Temporary
typedef enum Token {
	TK_FUNCTION = 1,
	TK_SHORT_ASG,
	TK_WHILE,
	TK_WAIT,
	TK_IN,
	TK_SIGNAL,
	TK_BROADCAST,
	TK_RETURN,
	TK_IF,
	TK_ELSE,
	TK_FOR,
	TK_SPAWN,
	TK_OR,
	TK_AND,
	TK_EQUAL,
	TK_LEQUAL,
	TK_GEQUAL,
	TK_NOT,
	TK_TRUE,
	TK_FALSE,
	TK_MONITOR,
	TK_PRIVATE,
	TK_INITIALIZER,
	TK_LOWER_ID,
	TK_UPPER_ID,
	TK_INTEGER,
	TK_FLOAT,
	TK_STRING,
} Token;

#endif
