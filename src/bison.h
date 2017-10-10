/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TK_IMMUTABLE = 258,
     TK_VALUE = 259,
     TK_VARIABLE = 260,
     TK_FUNCTION = 261,
     TK_WHILE = 262,
     TK_WAIT = 263,
     TK_IN = 264,
     TK_SIGNAL = 265,
     TK_BROADCAST = 266,
     TK_RETURN = 267,
     TK_IF = 268,
     TK_ELSE = 269,
     TK_FOR = 270,
     TK_SPAWN = 271,
     TK_TRUE = 272,
     TK_FALSE = 273,
     TK_MONITOR = 274,
     TK_PRIVATE = 275,
     TK_INITIALIZER = 276,
     TK_DEF_ASG = 277,
     TK_ADD_ASG = 278,
     TK_SUB_ASG = 279,
     TK_MUL_ASG = 280,
     TK_DIV_ASG = 281,
     TK_INTEGER = 282,
     TK_FLOAT = 283,
     TK_STRING = 284,
     TK_LOWER_ID = 285,
     TK_UPPER_ID = 286,
     TK_OR = 287,
     TK_AND = 288,
     TK_NEQUAL = 289,
     TK_EQUAL = 290,
     TK_GEQUAL = 291,
     TK_LEQUAL = 292,
     TK_NOT = 293
   };
#endif
/* Tokens.  */
#define TK_IMMUTABLE 258
#define TK_VALUE 259
#define TK_VARIABLE 260
#define TK_FUNCTION 261
#define TK_WHILE 262
#define TK_WAIT 263
#define TK_IN 264
#define TK_SIGNAL 265
#define TK_BROADCAST 266
#define TK_RETURN 267
#define TK_IF 268
#define TK_ELSE 269
#define TK_FOR 270
#define TK_SPAWN 271
#define TK_TRUE 272
#define TK_FALSE 273
#define TK_MONITOR 274
#define TK_PRIVATE 275
#define TK_INITIALIZER 276
#define TK_DEF_ASG 277
#define TK_ADD_ASG 278
#define TK_SUB_ASG 279
#define TK_MUL_ASG 280
#define TK_DIV_ASG 281
#define TK_INTEGER 282
#define TK_FLOAT 283
#define TK_STRING 284
#define TK_LOWER_ID 285
#define TK_UPPER_ID 286
#define TK_OR 287
#define TK_AND 288
#define TK_NEQUAL 289
#define TK_EQUAL 290
#define TK_GEQUAL 291
#define TK_LEQUAL 292
#define TK_NOT 293




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 31 "src/parser.y"
{
	// Tokens
	int ival;
	struct {
		Line line;
		union {
			int ival;
			double fval;
			const char* strval;
		};
	} literal;

	// Nonterminals
	Definition* definition;
	Id* id;
	Type* type;
	Block* block;
	Statement* statement;
	Variable* variable;
	Expression* expression;
	FunctionCall* function_call;
}
/* Line 1529 of yacc.c.  */
#line 148 "src/bison.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

