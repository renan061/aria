/*
 * TODO:
 *	- Change float to double?
 *	- Using a vector to deal with ids and strings is very inefficient.
 *		Therefore, another data strcture should be used instead.
 *	- Comentário usando //
 */

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
#include <assert.h>

#include "alloc.h"
#include "errs.h"
#include "parser.h"
#include "scanner.h"
#include "vector.h"

// scanner errors
#define ERR_FILE			"could not open file"
#define ERR_COMMENTARY_OPEN	"open commentary"
#define ERR_STRING_ESCAPE	"invalid escape"
#define ERR_STRING_OPEN		"open string"
#define ERR_STRING_LINE		"multiline string"

// auxiliary variables
static Vector* strs;
static unsigned int line = 1; // current line number

// auxiliary functions
static void idaction();
static const char* checkunique(const char* str);
static const char* copystr(const char* str, size_t len);
static const char* convertescapes(const char* str, size_t len);

%}

/*
 * Definitions
 */
digit		[0-9]
lower		[a-z]
upper		[A-Z]
wild		[_a-zA-Z0-9]

space		[ \t]
newline		\n
lower-id 	{lower}{wild}*
upper-id 	{upper}{wild}*
integer		{digit}+
float 		{digit}+"."{digit}+
string 		"\""(\\.|[^\\"])*"\""

%%

"/*"						BEGIN(COMMENTARY);
<COMMENTARY>"*/"			BEGIN(INITIAL);
<COMMENTARY>{newline}		line++;
<COMMENTARY><<EOF>>			scanner_error(line, ERR_COMMENTARY_OPEN);
<COMMENTARY>.				; /* empty */

{space} 		; /* empty */
{newline}		line++;

"function"		return yylval.ival = line, TK_FUNCTION;
":="			return yylval.ival = line, TK_ASSIGN;
"while"			return yylval.ival = line, TK_WHILE;
"wait"			return yylval.ival = line, TK_WAIT;
"in"			return yylval.ival = line, TK_IN;
"signal"		return yylval.ival = line, TK_SIGNAL;
"broadcast"		return yylval.ival = line, TK_BROADCAST;
"return"		return yylval.ival = line, TK_RETURN;
"if"			return yylval.ival = line, TK_IF;
"else"			return yylval.ival = line, TK_ELSE;
"for"			return yylval.ival = line, TK_FOR;
"spawn"			return yylval.ival = line, TK_SPAWN;
"or"			return yylval.ival = line, TK_OR;
"and"			return yylval.ival = line, TK_AND;
"=="			return yylval.ival = line, TK_EQUAL;
"<="			return yylval.ival = line, TK_LEQUAL;
">="			return yylval.ival = line, TK_GEQUAL;
"not"			return yylval.ival = line, TK_NOT;
"true"			return yylval.ival = line, TK_TRUE;
"false"			return yylval.ival = line, TK_FALSE;
"monitor"		return yylval.ival = line, TK_MONITOR;
"private"		return yylval.ival = line, TK_PRIVATE;
"initializer"	return yylval.ival = line, TK_INITIALIZER;

{lower-id}		return idaction(), TK_LOWER_ID;
{upper-id}		return idaction(), TK_UPPER_ID;

{integer}		{
					yylval.ival = strtoul(yytext, NULL, 10);
					return TK_INTEGER;
				}

{float}			{
					yylval.dval = strtod(yytext, NULL);
					return TK_FLOAT;
				}

{string}		{
					const char* temp = convertescapes(yytext, yyleng);
					yylval.strval = checkunique(temp);
				    if (!yylval.strval) {
				    	yylval.strval = temp;
				    	vector_append(strs, (void*)temp);
				    } else {
				    	free((char*)temp);
				    }
					return TK_STRING;
				}

"\""			scanner_error(line, ERR_STRING_OPEN);

. 				return yylval.ival = line, yytext[0];

%%

// initializes strs vector and sets yyin
void scanner_setup(char* file) {
	if (!(yyin = fopen(file, "r"))) {
    	scanner_error(line, ERR_FILE);
    }

	strs = vector_new();
}

// frees strs vector and closes yyin
void scanner_clean(void) {
	vector_destroy(strs);
	fclose(yyin);
}

// ==================================================
//
//	Auxiliary
//
// ==================================================

// used by lower-id and upper-id
static void idaction() {
	yylval.strval = checkunique(yytext);
    if (!yylval.strval) {
		yylval.strval = copystr(yytext, yyleng);
    }
}

// returns NULL if unique
static const char* checkunique(const char* str) {
	size_t size = vector_size(strs);
	for (int i = 0; i < size; i++) {
		char* s = vector_get(strs, i);
		if (!strcmp(str, s)) {
			return s;
		}
	}
	return NULL;
}

// also adds the string to strs
static const char* copystr(const char* str, size_t len) {
	char* newstr;
	MALLOC_ARRAY(newstr, char, len + 1);
    for (int i = 0; i < len; i++) {
    	newstr[i] = str[i];
    }
    newstr[len] = '\0';
    vector_append(strs, newstr);
    return newstr;
}

// TODO: Not working with LLVM constant definition "\\\\\ examples"
static const char* convertescapes(const char* str, size_t len) {
	// Calculating malloc size
	size_t size = 0;
	for (int i = 1; i < yyleng - 1; i++) {
		if (yytext[i] == '\\') {
			i++;
		}
		size++;
	}

	char* newstr;
	MALLOC_ARRAY(newstr, char, ++size);

	// Copying to the new string
	int k = 0;
	for (int i = 1; i < yyleng - 1; i++) {
		switch (yytext[i]) {
		case '\n':
			scanner_error(line, ERR_STRING_LINE);
			break; /* unnecessary */
		case '\\':
			switch (yytext[++i]) {
				case '"':	newstr[k++] = '"';	break;
				case 't':	newstr[k++] = '\t';	break;
				case 'n':	newstr[k++] = '\n';	break;
				case '\\':	newstr[k++] = '\\';	break;
				default: scanner_error(line, ERR_STRING_ESCAPE);
			}
			break;
		default:
			newstr[k++] = yytext[i];
		}
	}

	assert(k == size - 1);
	newstr[k] = '\0';
	return newstr;
}
