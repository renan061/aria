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

// extern void scanner_setup(void);
// extern void scanner_clean(void);
// extern unsigned int scanner_line(void);

// TODO: Temporary
typedef enum Token {
	TK_FUNCTION = 1,
	TK_TODO,
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
