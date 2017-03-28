#if !defined(parser_h)
#define parser_h

/*
 * TODO: tokens and yylval
 */
#include "bison.h"

/*
 * Runs the parser.
 */
extern int yyparse(void);

#endif
