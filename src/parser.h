#if !defined(parser_h)
#define parser_h

// Necessary because of nonterminal types inside bison.h
#include "ast.h"

/*
 * TODO: tokens and yylval.
 */
#include "bison.h"

/*
 * Runs the parser.
 */
extern int yyparse(void);

#endif
