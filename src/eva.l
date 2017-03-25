/*
 * Since the scanner does not utilize the 'yyunput'
 * and 'input' functions, it's necessary to provide
 * these options so no "defined but not used" warnings
 * are issued.
 */
%option nounput
%option noinput

/*
 * Since the compiler is only reading from one file,
 * there is no need to call (or define) the yywrap()
 * function. Therefore, the scanner assumes there are
 * no more files to scan when it reaches an end-of-file.
 */
%option noyywrap

/*
 * Exclusive start conditions (only rules qualified
 * with the start condition will be active).
 */
%x COMMENTARY

/*
 * C #include statements, #define statements and
 * variable/function declarations ONLY.
 */
%{
#include "scanner.h" // TODO: Remove later

// TODO: Move to errors file
#define ERR_COMMENT			"open commentary"
#define ERR_STRING_ESCAPE	"invalid escape"			// TODO: Unused
#define ERR_STRING_OPEN		"open string"
#define ERR_STRING_LINE		"multiline string"			// TODO: Unused
#define ERR_INTERNAL		"scanner internal error"	// TODO: Unused

static unsigned int line = 1; // current line number

// TODO: Remove
static void error(char* err);

%}

/* Definitions */
space		[ \t]
newline		\n

lower-id 	[a-z][_a-zA-Z0-9]*
upper-id 	[A-Z][_a-zA-Z0-9]*

integer		[0-9]+
float 		[0-9]+"."[0-9]+
string 		"\""(\\.|[^\\"])*"\""

%%

"/*"						BEGIN(COMMENTARY);
<COMMENTARY>"*/"			BEGIN(INITIAL);
<COMMENTARY>{newline}		line++;
<COMMENTARY><<EOF>>			error(ERR_COMMENT);
<COMMENTARY>.				; /* empty */

{space} 		; /* empty */
{newline}		line++;

"function"		{ 
					// TODO: Remove {} and write
					// return yylval.ival = line, TK_FUNCTION;
					// return (yylval.ival = line, TK_FUNCTION);
					return TK_FUNCTION;
				}
":="			{ return TK_TODO;			}
"while"			{ return TK_WHILE;			}
"wait"			{ return TK_WAIT;			}
"in"			{ return TK_IN;				}
"signal"		{ return TK_SIGNAL;			}
"broadcast"		{ return TK_BROADCAST;		}
"return"		{ return TK_RETURN;			}
"if"			{ return TK_IF;				}
"else"			{ return TK_ELSE;			}
"for"			{ return TK_FOR;			}
"spawn"			{ return TK_SPAWN;			}
"or"			{ return TK_OR;				}
"and"			{ return TK_AND;			}
"=="			{ return TK_EQUAL;			}
"<="			{ return TK_LEQUAL;			}
">="			{ return TK_GEQUAL;			}
"not"			{ return TK_NOT;			}
"true"			{ return TK_TRUE;			}
"false"			{ return TK_FALSE;			}
"monitor"		{ return TK_MONITOR;		}
"private"		{ return TK_PRIVATE;		}
"initializer"	{ return TK_INITIALIZER;	}

{lower-id}		{
					return TK_LOWER_ID;
				}

{upper-id}		{
					return TK_UPPER_ID;
				}

{integer}		{
					return TK_INTEGER;
				}

{float}			{
					return TK_FLOAT;
				}

{string}		{
					return TK_STRING;
				}

"\""			{
					error(ERR_STRING_OPEN);
				}

. 				{
					return yytext[0];
				}
%%

static void error(char* err) {
	// TODO: Remove this function and call external
	// "errors.h" function directly
}
