#if !defined(scanner_h)
#define scanner_h

/*
 * The token type to be used in other modules.
 */
typedef unsigned int Token;

/*
 * Used by the parser to get the next token.
 */
extern int yylex(void);

/*
 * TODO
 */
#define SCANNER_BOOLEAN 0
#define SCANNER_INTEGER 1
#define SCANNER_FLOAT 2
#define SCANNER_STRING 3
extern const char* primitive_types[4];

/*
 * Setup and Clean should be called before starting
 * to use the scanner and after finishing doing so,
 * respectively.
 */
extern void scanner_setup(const char* file);
extern void scanner_clean(void);

#endif
