#if !defined(parser_h)
#define parser_h

#include "ast.h"	// necessary because of nonterminal types inside bison.h
#include "bison.h"	// necessary because of yylval and tokens inside scanner.l

// runs the parser
extern int yyparse(void);

#endif
