%x IN_COMMENT
%option nounput

/* C #defines and declarations */
%{
	#define YY_NO_INPUT

	#define ERR_COMMENT			"open commentary"
	#define ERR_STRING_ESCAPE	"invalid escape"			// TODO: Unused
	#define ERR_STRING_OPEN		"open string"
	#define ERR_STRING_LINE		"multiline string"			// TODO: Unused
	#define ERR_INTERNAL		"scanner internal error"	// TODO: Unused

	// Current line number
	static unsigned int line = 1;

	static void error(char* err);
%}

/* Definitions */
space		[ \t]
newline		\n
wild-id		[A-Za-z_0-9]*
lower-id 	[a-z_]wild-id
upper-id 	[A-Z]wild-id
integer		[0-9]+
float 		[0-9]+"."[0-9]+
string 		"\""(\\.|[^\\"])*"\""

%%

"/*"						BEGIN(IN_COMMENT);
<IN_COMMENT>"*/"			BEGIN(INITIAL);
<IN_COMMENT>{newline}		line++;
<IN_COMMENT><<EOF>>			error(ERR_COMMENT);
<IN_COMMENT>.				; /* empty */

{space} 	; /* empty */

{newline}	line++;

"function"		{ return TK_FUNCTION;		}
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

{lower-id}	{
				return TK_LOWER_ID;
			}

{upper-id}	{
				return TK_UPPER_ID;
			}

{integer}	{
				return TK_INTEGER;
			}

{float}		{
				return TK_FLOAT;
			}

{string}	{
				return TK_STRING;
			}

"\""		{
				error(ERR_STRING_OPEN);
			}

. 			{
				return yytext[0];
			}
%%
