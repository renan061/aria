#if !defined(scanner_h)
#define scanner_h

/*
 * TODO: Doc
 */
typedef unsigned int Token;

/*
 * Used by the parser to get the next token.
 */
extern int yylex(void);

/*
 * Setup and Clean should be called before starting
 * to use the scanner and after finishing doing so,
 * respectively.
 */
extern void scanner_setup(char* file);
extern void scanner_clean(void);

#endif
