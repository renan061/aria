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
%x LINE_COMMENTARY BLOCK_COMMENTARY

/*
 * C #include statements, #define statements and
 * variable/function declarations ONLY.
 */
%{
#include <assert.h>

#include "alloc.h"
#include "ast.h"
#include "errs.h"
#include "parser.h" // for the tokens
#include "scanner.h"
#include "vector.h"

// scanner errors
#define ERR_FILE            "could not open file"
#define ERR_COMMENTARY_OPEN "open commentary"
#define ERR_STRING_ESCAPE   "invalid escape"
#define ERR_STRING_OPEN     "open string"
#define ERR_STRING_LINE     "multiline string"

// auxiliary macro
#define TOKEN(t) yylval.ival = line, t

// auxiliary variables
static Vector* strs;
static unsigned int line = 1; // current line number

// auxiliary functions
static void idaction();
static const char* checkunique(const char*);
static const char* copystr(const char*, size_t);
static const char* convertescapes(const char*, size_t);

%}

/*
 * Definitions
 */
digit       [0-9]
lower       [a-z]
upper       [A-Z]
wild        [_a-zA-Z0-9]

space       [ \t]
newline     \n
lower-id    {lower}{wild}*
upper-id    {upper}{wild}*
integer     {digit}+
float       {digit}+"."{digit}+
string      "\""(\\.|[^\\"])*"\""

%%

"//"                            BEGIN(LINE_COMMENTARY);
<LINE_COMMENTARY>{newline}      line++, BEGIN(INITIAL);
<LINE_COMMENTARY>.              ; /* empty */

"/*"                            BEGIN(BLOCK_COMMENTARY);
<BLOCK_COMMENTARY>"*/"          BEGIN(INITIAL);
<BLOCK_COMMENTARY>{newline}     line++;
<BLOCK_COMMENTARY><<EOF>>       scanner_error(line, ERR_COMMENTARY_OPEN);
<BLOCK_COMMENTARY>.             ; /* empty */

{space}         ; /* empty */
{newline}       line++;

"=="            return TOKEN(TK_EQUAL); // operators
"!="            return TOKEN(TK_NEQUAL);
"<="            return TOKEN(TK_LEQUAL);
">="            return TOKEN(TK_GEQUAL);
"not"           return TOKEN(TK_NOT);
"and"           return TOKEN(TK_AND);
"or"            return TOKEN(TK_OR);
":="            return TOKEN(TK_DEF_ASG);
"+="            return TOKEN(TK_ADD_ASG);
"-="            return TOKEN(TK_SUB_ASG);
"*="            return TOKEN(TK_MUL_ASG);
"/="            return TOKEN(TK_DIV_ASG);

"acquire"       return TOKEN(TK_ACQUIRE); // keywords
"as"            return TOKEN(TK_AS);
"broadcast"     return TOKEN(TK_BROADCAST);
"else"          return TOKEN(TK_ELSE);
"false"         return TOKEN(TK_FALSE);
"for"           return TOKEN(TK_FOR);
"function"      return TOKEN(TK_FUNCTION);
"if"            return TOKEN(TK_IF);
"Immutable"     return TOKEN(TK_IMMUTABLE);
"in"            return TOKEN(TK_IN);
"initializer"   return TOKEN(TK_INITIALIZER);
"interface"     return TOKEN(TK_INTERFACE);
"monitor"       return TOKEN(TK_MONITOR);
"private"       return TOKEN(TK_PRIVATE);
"release"       return TOKEN(TK_RELEASE);
"return"        return TOKEN(TK_RETURN);
"signal"        return TOKEN(TK_SIGNAL);
"spawn"         return TOKEN(TK_SPAWN);
"structure"     return TOKEN(TK_STRUCTURE);
"true"          return TOKEN(TK_TRUE);
"value"         return TOKEN(TK_VALUE);
"variable"      return TOKEN(TK_VARIABLE);
"wait"          return TOKEN(TK_WAIT);
"while"         return TOKEN(TK_WHILE);

{lower-id}      return idaction(), TK_LOWER_ID;
{upper-id}      return idaction(), TK_UPPER_ID;

{integer}       {
                    yylval.literal.ival = strtoul(yytext, NULL, 10);
                    return TK_INTEGER;
                }

{float}         {
                    yylval.literal.fval = strtod(yytext, NULL);
                    return TK_FLOAT;
                }

{string}        {
                    const char* temp = convertescapes(yytext, yyleng);
                    yylval.literal.strval = checkunique(temp);
                    if (!yylval.literal.strval) {
                        yylval.literal.strval = temp;
                        vector_append(strs, (void*)temp);
                    } else {
                        free((char*)temp);
                    }
                    return TK_STRING;
                }

"\""            scanner_error(line, ERR_STRING_OPEN);

.               return TOKEN(yytext[0]);

%%

const char* scanner_native[SCANNER_NATIVE_SIZE];

// initializes strs vector and sets yyin
void scanner_setup(const char* file) {
    if (!(yyin = fopen(file, "r"))) {
        scanner_error(line, ERR_FILE);
    }
    strs = vector_new();

    scanner_native[SCANNER_NATIVE_BOOLEAN] = "Boolean";
    scanner_native[SCANNER_NATIVE_INTEGER] = "Integer";
    scanner_native[SCANNER_NATIVE_FLOAT] = "Float";
    scanner_native[SCANNER_NATIVE_STRING] = "String";
    scanner_native[SCANNER_NATIVE_CONDITION_QUEUE] = "ConditionQueue";
    scanner_native[SCANNER_NATIVE_SELF] = "self";
    scanner_native[SCANNER_NATIVE_MAIN] = "main";

    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_BOOLEAN]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_INTEGER]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_FLOAT]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_STRING]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_CONDITION_QUEUE]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_SELF]);
    vector_append(strs, (void*) scanner_native[SCANNER_NATIVE_MAIN]);
}

// frees strs vector and closes yyin
void scanner_clean(void) {
    vector_destroy(strs);
    fclose(yyin);
}

// ==================================================
//
//  Auxiliary
//
// ==================================================

// TODO: Using a vector to deal with ids and strings is very inefficient.
// Another data structure should be used instead.

// used by lower-id and upper-id
static void idaction() {
    const char* str = checkunique(yytext);
    if (!str) {
        str = copystr(yytext, yyleng);
    }
    yylval.id = ast_id(line, str);
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
                case '"':   newstr[k++] = '"';  break;
                case 't':   newstr[k++] = '\t'; break;
                case 'n':   newstr[k++] = '\n'; break;
                case '\\':  newstr[k++] = '\\'; break;
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
