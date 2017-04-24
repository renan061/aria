#if !defined(parser_h)
#define parser_h

#include "ast.h"	// Necessary because of nonterminal types inside bison.h
#include "bison.h"	// Necessary because of yylval and tokens inside scanner.l

/*
 * Runs the parser.
 */
extern int yyparse(void);

#endif
