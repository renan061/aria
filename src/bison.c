/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



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




/* Copy the first part of user declarations.  */
#line 9 "src/parser.y"

	#include <assert.h>
	#include <stdlib.h>

	#include "ast.h"
	#include "errs.h"
	#include "scanner.h"

	// Auxiliary macro to use with lists
	#define APPEND(type, assignable, list, elem); \
		if (!list) { \
			assignable = elem; \
		} else { \
			type* e; \
			for (e = assignable = list; e->next; e = e->next); \
			e->next = elem; \
		} \

	static void yyerror(const char* err);


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

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
/* Line 193 of yacc.c.  */
#line 216 "parser.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 229 "parser.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   302

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  39
/* YYNRULES -- Number of rules.  */
#define YYNRULES  104
/* YYNRULES -- Number of states.  */
#define YYNSTATES  201

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   293

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       7,     8,    50,    48,    55,    49,    54,    51,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    53,    10,
      44,     9,    45,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     5,     2,     6,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     2,     4,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    46,    47,    52
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     6,     9,    12,    14,    16,    19,
      25,    29,    32,    35,    37,    39,    46,    51,    55,    57,
      61,    66,    70,    71,    74,    77,    80,    82,    85,    87,
      89,    91,    97,   100,   103,   105,   108,   113,   117,   120,
     122,   123,   129,   132,   136,   140,   144,   148,   152,   154,
     159,   163,   167,   171,   175,   179,   183,   187,   191,   195,
     199,   203,   207,   210,   213,   215,   217,   219,   221,   225,
     227,   229,   231,   233,   235,   239,   244,   249,   256,   261,
     265,   266,   269,   272,   275,   277,   279,   282,   284,   288,
     289,   291,   293,   297,   301,   303,   307,   309,   313,   314,
     318,   320,   324,   326,   328
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      57,     0,    -1,    58,    -1,    -1,    58,    59,    -1,    62,
      10,    -1,    65,    -1,    66,    -1,    13,    90,    -1,    38,
      53,    67,     9,    77,    -1,    38,     9,    77,    -1,    12,
      61,    -1,    13,    61,    -1,    62,    -1,    63,    -1,    14,
      38,    91,    53,    67,    68,    -1,    14,    38,    91,    68,
      -1,    27,    39,    81,    -1,    39,    -1,     5,    67,     6,
      -1,    11,     5,    67,     6,    -1,     3,    69,     4,    -1,
      -1,    69,    70,    -1,    60,    10,    -1,    94,    10,    -1,
      71,    -1,    72,    10,    -1,    73,    -1,    75,    -1,    80,
      -1,    16,    23,    77,    17,    77,    -1,    18,    77,    -1,
      19,    77,    -1,    20,    -1,    20,    77,    -1,    21,    77,
      68,    74,    -1,    15,    77,    68,    -1,    24,    68,    -1,
      68,    -1,    -1,    22,    21,    77,    68,    74,    -1,    22,
      68,    -1,    76,     9,    77,    -1,    76,    31,    77,    -1,
      76,    32,    77,    -1,    76,    33,    77,    -1,    76,    34,
      77,    -1,    38,    -1,    78,     5,    77,     6,    -1,    77,
      40,    77,    -1,    77,    41,    77,    -1,    77,    43,    77,
      -1,    77,    42,    77,    -1,    77,    47,    77,    -1,    77,
      46,    77,    -1,    77,    44,    77,    -1,    77,    45,    77,
      -1,    77,    48,    77,    -1,    77,    49,    77,    -1,    77,
      50,    77,    -1,    77,    51,    77,    -1,    49,    77,    -1,
      52,    77,    -1,    78,    -1,    79,    -1,    76,    -1,    80,
      -1,     7,    77,     8,    -1,    25,    -1,    26,    -1,    35,
      -1,    36,    -1,    37,    -1,     5,    87,     6,    -1,    11,
       5,    87,     6,    -1,    38,     7,    86,     8,    -1,    78,
      54,    38,     7,    86,     8,    -1,    67,     7,    86,     8,
      -1,     3,    82,     4,    -1,    -1,    82,    83,    -1,    60,
      10,    -1,    64,    10,    -1,    84,    -1,    85,    -1,    28,
      65,    -1,    65,    -1,    29,    91,    68,    -1,    -1,    87,
      -1,    77,    -1,    87,    55,    77,    -1,    38,    53,    67,
      -1,    38,    -1,    89,    55,    38,    -1,    88,    -1,    89,
      55,    88,    -1,    -1,     7,    92,     8,    -1,    93,    -1,
      92,    55,    93,    -1,    90,    -1,    64,    -1,    38,    30,
      77,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   118,   118,   126,   129,   136,   141,   145,   158,   172,
     178,   185,   193,   201,   205,   212,   216,   223,   237,   241,
     245,   280,   288,   291,   298,   302,   306,   319,   323,   330,
     334,   338,   342,   346,   350,   354,   361,   368,   373,   377,
     401,   404,   412,   419,   423,   427,   431,   435,   448,   452,
     465,   469,   473,   477,   481,   485,   489,   493,   497,   501,
     505,   509,   513,   517,   521,   528,   532,   536,   540,   547,
     551,   555,   559,   563,   567,   571,   584,   588,   592,   605,
     613,   616,   623,   627,   631,   635,   642,   646,   653,   668,
     671,   679,   683,   693,   702,   706,   714,   725,   739,   742,
     749,   753,   760,   770,   779
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "'{'", "'}'", "'['", "']'", "'('", "')'",
  "'='", "';'", "TK_IMMUTABLE", "TK_VALUE", "TK_VARIABLE", "TK_FUNCTION",
  "TK_WHILE", "TK_WAIT", "TK_IN", "TK_SIGNAL", "TK_BROADCAST", "TK_RETURN",
  "TK_IF", "TK_ELSE", "TK_FOR", "TK_SPAWN", "TK_TRUE", "TK_FALSE",
  "TK_MONITOR", "TK_PRIVATE", "TK_INITIALIZER", "TK_DEF_ASG", "TK_ADD_ASG",
  "TK_SUB_ASG", "TK_MUL_ASG", "TK_DIV_ASG", "TK_INTEGER", "TK_FLOAT",
  "TK_STRING", "TK_LOWER_ID", "TK_UPPER_ID", "TK_OR", "TK_AND",
  "TK_NEQUAL", "TK_EQUAL", "'<'", "'>'", "TK_GEQUAL", "TK_LEQUAL", "'+'",
  "'-'", "'*'", "'/'", "TK_NOT", "':'", "'.'", "','", "$accept", "program",
  "file_definition_list", "file_definition", "variable_declaration",
  "lower_id_optional_type_expression", "variable_definition_value",
  "variable_definition_variable", "variable_definition",
  "function_definition", "monitor_definition", "type", "block",
  "block_content_list", "block_content", "statement", "simple_statement",
  "compound_statement", "else", "statement_assignment", "variable",
  "expression", "primary_expression", "literal", "function_call",
  "monitor_body", "monitor_body_definition_list",
  "monitor_body_definition", "method_definition", "constructor_definition",
  "expression_list0", "expression_list1", "lower_id_type", "lower_ids",
  "lower_ids_type", "parameter_list", "parameters", "parameter",
  "block_variable_definition", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   123,   125,    91,    93,    40,    41,    61,
      59,   258,   259,   260,   261,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   288,   289,   290,    60,    62,   291,   292,    43,    45,
      42,    47,   293,    58,    46,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    57,    58,    58,    59,    59,    59,    60,    61,
      61,    62,    63,    64,    64,    65,    65,    66,    67,    67,
      67,    68,    69,    69,    70,    70,    70,    71,    71,    72,
      72,    72,    72,    72,    72,    72,    73,    73,    73,    73,
      74,    74,    74,    75,    75,    75,    75,    75,    76,    76,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    78,    78,    78,    78,    79,
      79,    79,    79,    79,    79,    79,    80,    80,    80,    81,
      82,    82,    83,    83,    83,    83,    84,    84,    85,    86,
      86,    87,    87,    88,    89,    89,    90,    90,    91,    91,
      92,    92,    93,    94,    94
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     2,     2,     1,     1,     2,     5,
       3,     2,     2,     1,     1,     6,     4,     3,     1,     3,
       4,     3,     0,     2,     2,     2,     1,     2,     1,     1,
       1,     5,     2,     2,     1,     2,     4,     3,     2,     1,
       0,     5,     2,     3,     3,     3,     3,     3,     1,     4,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     3,     4,     4,     6,     4,     3,
       0,     2,     2,     2,     1,     1,     2,     1,     3,     0,
       1,     1,     3,     3,     1,     3,     1,     3,     0,     3,
       1,     3,     1,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,     2,     1,     0,     0,     0,     4,     0,     6,
       7,     0,    11,    98,     0,     5,     0,     0,     0,     0,
      80,    17,     0,     0,     0,    69,    70,    71,    72,    73,
      48,    18,     0,     0,     0,    66,    10,    64,    65,    67,
       0,     0,     0,    94,    96,     0,   102,     0,   100,    22,
       0,    16,     0,     0,    91,     0,     0,     0,    89,    62,
      63,    89,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    79,     0,     0,    98,     0,
      13,    14,     0,    87,    81,    84,    85,    19,    74,     0,
      68,     0,     0,     0,    90,     0,    50,    51,    53,    52,
      56,    57,    55,    54,    58,    59,    60,    61,     0,     0,
       0,     9,    93,    95,    97,   101,    21,     0,     0,     0,
       0,    34,     0,     0,    48,     0,   103,    39,    23,    26,
       0,    28,    29,    66,     0,    67,     0,    15,    94,    12,
       8,    86,     0,    82,    83,    92,    20,    75,    76,    78,
      49,    89,     0,     0,    32,    33,    35,     0,    38,     0,
      24,    27,     0,     0,     0,     0,     0,    25,     0,    88,
       0,    37,     0,    40,   104,    43,    44,    45,    46,    47,
      93,    77,     0,     0,    36,    31,     0,    42,     0,    40,
      41
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     7,    89,    12,    90,    91,    92,     9,
      10,    34,    51,    83,   138,   139,   140,   141,   194,   142,
      35,    54,    37,    38,    39,    21,    52,    94,    95,    96,
     103,   104,    44,    45,    46,    19,    47,    48,   146
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -75
static const yytype_int16 yypact[] =
{
     -75,    24,    17,   -75,    -8,    -3,    -1,   -75,    31,   -75,
     -75,    16,   -75,    35,    42,   -75,    69,     1,    28,     8,
     -75,   -75,    69,    69,    65,   -75,   -75,   -75,   -75,   -75,
      70,   -75,    69,    69,    72,   -75,   220,    10,   -75,   -75,
       1,    67,    64,    29,   -75,    26,   -75,     5,   -75,   -75,
       1,   -75,   137,    14,   220,    12,   154,    69,    69,   -75,
     -75,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    46,    79,     1,    69,     1,
      48,   -75,    28,   203,    84,   -75,    52,    77,    35,    82,
     -75,   -75,    92,   -75,   -75,   -75,   -75,   -75,   -75,    69,
     -75,    27,    13,    88,    54,    95,   231,   241,   251,   251,
     123,   123,   123,   123,   -28,   -28,   -75,   -75,   142,   103,
     106,   220,   -75,    29,   -75,   -75,   -75,    69,    94,    69,
      69,    69,    69,    84,    -2,   110,   -75,   -75,   -75,   -75,
     112,   -75,   -75,    66,    10,   113,   115,   -75,    18,   -75,
     -75,   -75,    84,   -75,   -75,   220,   -75,   -75,   -75,   -75,
     -75,    69,    86,    69,   220,   220,   220,    86,   -75,    69,
     -75,   -75,    69,    69,    69,    69,    69,   -75,     1,   -75,
     130,   -75,   208,   117,   220,   220,   220,   220,   220,   220,
      64,   -75,    69,    11,   -75,   220,    69,   -75,    86,   117,
     -75
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -75,   -75,   -75,   -75,    57,    56,   141,   -75,    61,   -50,
     -75,   -14,   -74,   -75,   -75,   -75,   -75,   -75,   -54,   -75,
      63,   -16,    71,   -75,    78,   -75,   -75,   -75,   -75,   -75,
     -60,   -18,    75,   -75,    89,    90,   -75,    81,   -75
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -31
static const yytype_int16 yytable[] =
{
      36,   105,    93,    42,    55,    58,    40,    56,    53,   137,
     147,    49,    41,    81,    49,    74,    59,    60,    98,   157,
      97,    61,    72,    73,     3,    16,    76,    16,   169,     4,
      11,     5,   196,   156,    61,    13,    84,   151,    14,   102,
      31,    15,    18,   101,     6,    20,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   168,
      82,    50,   121,   120,    75,   122,    43,    99,    99,    17,
      57,   178,    77,    78,    22,   172,    23,    58,   179,    61,
      24,    80,    79,   155,   119,    97,   123,    49,   181,    49,
     148,     5,   153,   183,    25,    26,   158,   173,   174,   175,
     176,   180,   154,   159,    27,    28,    29,    30,    31,    99,
     161,   162,   156,   164,   165,   166,   167,   163,    32,   197,
     170,    33,   171,   -30,   199,   177,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,   191,   193,
     135,    85,   149,     8,   136,   200,   143,   182,   160,     4,
      86,     5,     0,   184,   144,   124,   185,   186,   187,   188,
     189,   145,   100,   125,   190,    87,    88,   -31,   -31,   -31,
     -31,    70,    71,    72,    73,   150,   195,     0,   152,     0,
     198,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    49,   126,    22,     0,
      23,     0,     0,     0,    24,     4,    86,     0,   127,   128,
       0,   129,   130,   131,   132,   192,     0,   133,    25,    26,
       0,     0,     0,     0,     0,     0,     0,     0,    27,    28,
      29,   134,    31,     0,     0,     0,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,   -31,   -31,    66,    67,    68,    69,    70,
      71,    72,    73
};

static const yytype_int16 yycheck[] =
{
      16,    61,    52,    17,    22,     7,     5,    23,    22,    83,
      84,     3,    11,     8,     3,     5,    32,    33,     6,     6,
       6,     7,    50,    51,     0,     9,    40,     9,    30,    12,
      38,    14,    21,     6,     7,    38,    50,    87,    39,    57,
      39,    10,     7,    57,    27,     3,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,   133,
      55,    53,    78,    77,    54,    79,    38,    55,    55,    53,
       5,    53,     5,     9,     5,     9,     7,     7,   152,     7,
      11,    55,    53,    99,    38,     6,    38,     3,   162,     3,
      38,    14,    10,   167,    25,    26,     8,    31,    32,    33,
      34,   161,    10,     8,    35,    36,    37,    38,    39,    55,
       7,   127,     6,   129,   130,   131,   132,    23,    49,   193,
      10,    52,    10,    10,   198,    10,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     8,    22,
      83,     4,    86,     2,    83,   199,    83,   163,     6,    12,
      13,    14,    -1,   169,    83,    80,   172,   173,   174,   175,
     176,    83,     8,    82,   178,    28,    29,    44,    45,    46,
      47,    48,    49,    50,    51,    86,   192,    -1,    88,    -1,
     196,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    13,    -1,    15,    16,
      -1,    18,    19,    20,    21,    17,    -1,    24,    25,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    57,    58,     0,    12,    14,    27,    59,    62,    65,
      66,    38,    61,    38,    39,    10,     9,    53,     7,    91,
       3,    81,     5,     7,    11,    25,    26,    35,    36,    37,
      38,    39,    49,    52,    67,    76,    77,    78,    79,    80,
       5,    11,    67,    38,    88,    89,    90,    92,    93,     3,
      53,    68,    82,    67,    77,    87,    77,     5,     7,    77,
      77,     7,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     5,    54,    67,     5,     9,    53,
      55,     8,    55,    69,    67,     4,    13,    28,    29,    60,
      62,    63,    64,    65,    83,    84,    85,     6,     6,    55,
       8,    67,    87,    86,    87,    86,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    38,
      67,    77,    67,    38,    88,    93,     4,    15,    16,    18,
      19,    20,    21,    24,    38,    60,    64,    68,    70,    71,
      72,    73,    75,    76,    78,    80,    94,    68,    38,    61,
      90,    65,    91,    10,    10,    77,     6,     6,     8,     8,
       6,     7,    77,    23,    77,    77,    77,    77,    68,    30,
      10,    10,     9,    31,    32,    33,    34,    10,    53,    68,
      86,    68,    77,    68,    77,    77,    77,    77,    77,    77,
      67,     8,    17,    22,    74,    77,    21,    68,    77,    68,
      74
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 119 "src/parser.y"
    {
			ast_set((yyvsp[(1) - (1)].definition));
		;}
    break;

  case 3:
#line 126 "src/parser.y"
    {
			(yyval.definition) = NULL;
		;}
    break;

  case 4:
#line 130 "src/parser.y"
    {
			APPEND(Definition, (yyval.definition), (yyvsp[(1) - (2)].definition), (yyvsp[(2) - (2)].definition));
		;}
    break;

  case 5:
#line 137 "src/parser.y"
    {
			(yyvsp[(1) - (2)].definition)->variable.variable->global = true;
			(yyval.definition) = (yyvsp[(1) - (2)].definition);
		;}
    break;

  case 6:
#line 142 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 7:
#line 146 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 8:
#line 159 "src/parser.y"
    {
			(yyvsp[(2) - (2)].definition)->variable.variable->value = false;
			(yyval.definition) = (yyvsp[(2) - (2)].definition);
		;}
    break;

  case 9:
#line 173 "src/parser.y"
    {
			Variable* variable = ast_variable_id((yyvsp[(1) - (5)].id));
			variable->type = (yyvsp[(3) - (5)].type);
			(yyval.definition) = ast_definition_variable(variable, (yyvsp[(5) - (5)].expression));
		;}
    break;

  case 10:
#line 179 "src/parser.y"
    {
			(yyval.definition) = ast_definition_variable(ast_variable_id((yyvsp[(1) - (3)].id)), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 11:
#line 186 "src/parser.y"
    {
			(yyvsp[(2) - (2)].definition)->variable.variable->value = true;
			(yyval.definition) = (yyvsp[(2) - (2)].definition);
		;}
    break;

  case 12:
#line 194 "src/parser.y"
    {
			(yyvsp[(2) - (2)].definition)->variable.variable->value = false;
			(yyval.definition) = (yyvsp[(2) - (2)].definition);
		;}
    break;

  case 13:
#line 202 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 14:
#line 206 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 15:
#line 213 "src/parser.y"
    {
			(yyval.definition) = ast_definition_function((yyvsp[(2) - (6)].id), (yyvsp[(3) - (6)].definition), (yyvsp[(5) - (6)].type), (yyvsp[(6) - (6)].block));
		;}
    break;

  case 16:
#line 217 "src/parser.y"
    {
			(yyval.definition) = ast_definition_function((yyvsp[(2) - (4)].id), (yyvsp[(3) - (4)].definition), ast_type_void(), (yyvsp[(4) - (4)].block));
		;}
    break;

  case 17:
#line 224 "src/parser.y"
    {
			(yyval.definition) = ast_definition_type(ast_type_monitor((yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].definition)));
		;}
    break;

  case 18:
#line 238 "src/parser.y"
    {
			(yyval.type) = ast_type_id((yyvsp[(1) - (1)].id));
		;}
    break;

  case 19:
#line 242 "src/parser.y"
    {
			(yyval.type) = ast_type_array((yyvsp[(2) - (3)].type));
		;}
    break;

  case 20:
#line 246 "src/parser.y"
    {
			(yyval.type) = ast_type_array((yyvsp[(3) - (4)].type));

			// TODO: Recursive immutability
			Type* type = (yyval.type);
			while (type) {
				switch (type->tag) {
				case TYPE_VOID:
					type = NULL;
					break;
				case TYPE_ID:
					type = NULL;
					break;
				case TYPE_ARRAY:
					type->immutable = true;
					type = type->array;
					break;
				case TYPE_MONITOR:
					type = NULL;
					break;
				default:
					assert(0); // TODO
				}
			}
		;}
    break;

  case 21:
#line 281 "src/parser.y"
    {
			(yyval.block) = ast_block((yyvsp[(1) - (3)].ival), (yyvsp[(2) - (3)].block));
		;}
    break;

  case 22:
#line 288 "src/parser.y"
    {
			(yyval.block) = NULL;
		;}
    break;

  case 23:
#line 292 "src/parser.y"
    {
			APPEND(Block, (yyval.block), (yyvsp[(1) - (2)].block), (yyvsp[(2) - (2)].block));
		;}
    break;

  case 24:
#line 299 "src/parser.y"
    {
			(yyval.block) = ast_block_definition((yyvsp[(1) - (2)].definition));
		;}
    break;

  case 25:
#line 303 "src/parser.y"
    {
			(yyval.block) = ast_block_definition((yyvsp[(1) - (2)].definition));
		;}
    break;

  case 26:
#line 307 "src/parser.y"
    {
			(yyval.block) = ast_block_statement((yyvsp[(1) - (1)].statement));
		;}
    break;

  case 27:
#line 320 "src/parser.y"
    {
			(yyval.statement) = (yyvsp[(1) - (2)].statement);
		;}
    break;

  case 28:
#line 324 "src/parser.y"
    {
			(yyval.statement) = (yyvsp[(1) - (1)].statement);
		;}
    break;

  case 29:
#line 331 "src/parser.y"
    {
			(yyval.statement) = (yyvsp[(1) - (1)].statement);
		;}
    break;

  case 30:
#line 335 "src/parser.y"
    {
			(yyval.statement) = ast_statement_function_call((yyvsp[(1) - (1)].function_call));
		;}
    break;

  case 31:
#line 339 "src/parser.y"
    {
			(yyval.statement) = ast_statement_wait_for_in((yyvsp[(1) - (5)].ival), (yyvsp[(3) - (5)].expression), (yyvsp[(5) - (5)].expression));
		;}
    break;

  case 32:
#line 343 "src/parser.y"
    {
			(yyval.statement) = ast_statement_signal((yyvsp[(1) - (2)].ival), (yyvsp[(2) - (2)].expression));
		;}
    break;

  case 33:
#line 347 "src/parser.y"
    {
			(yyval.statement) = ast_statement_broadcast((yyvsp[(1) - (2)].ival), (yyvsp[(2) - (2)].expression));
		;}
    break;

  case 34:
#line 351 "src/parser.y"
    {
			(yyval.statement) = ast_statement_return((yyvsp[(1) - (1)].ival), NULL);
		;}
    break;

  case 35:
#line 355 "src/parser.y"
    {
			(yyval.statement) = ast_statement_return((yyvsp[(1) - (2)].ival), (yyvsp[(2) - (2)].expression));
		;}
    break;

  case 36:
#line 362 "src/parser.y"
    {
			(yyval.statement) = ((yyvsp[(4) - (4)].block))
				? ast_statement_if_else((yyvsp[(1) - (4)].ival), (yyvsp[(2) - (4)].expression), (yyvsp[(3) - (4)].block), (yyvsp[(4) - (4)].block))
				: ast_statement_if((yyvsp[(1) - (4)].ival), (yyvsp[(2) - (4)].expression), (yyvsp[(3) - (4)].block))
				;
		;}
    break;

  case 37:
#line 369 "src/parser.y"
    {
			(yyval.statement) = ast_statement_while((yyvsp[(1) - (3)].ival), (yyvsp[(2) - (3)].expression), (yyvsp[(3) - (3)].block));
		;}
    break;

  case 38:
#line 374 "src/parser.y"
    {
			(yyval.statement) = ast_statement_spawn((yyvsp[(1) - (2)].ival), (yyvsp[(2) - (2)].block));
		;}
    break;

  case 39:
#line 378 "src/parser.y"
    {
			(yyval.statement) = ast_statement_block((yyvsp[(1) - (1)].block));
		;}
    break;

  case 40:
#line 401 "src/parser.y"
    {
			(yyval.block) = NULL;
		;}
    break;

  case 41:
#line 405 "src/parser.y"
    {			
			Statement* statement = ((yyvsp[(5) - (5)].block))
				? ast_statement_if_else((yyvsp[(2) - (5)].ival), (yyvsp[(3) - (5)].expression), (yyvsp[(4) - (5)].block), (yyvsp[(5) - (5)].block))
				: ast_statement_if((yyvsp[(2) - (5)].ival), (yyvsp[(3) - (5)].expression), (yyvsp[(4) - (5)].block))
				;
			(yyval.block) = ast_block(statement->line, ast_block_statement(statement));
		;}
    break;

  case 42:
#line 413 "src/parser.y"
    {
			(yyval.block) = (yyvsp[(2) - (2)].block);
		;}
    break;

  case 43:
#line 420 "src/parser.y"
    {
			(yyval.statement) = ast_statement_assignment((yyvsp[(2) - (3)].ival), '=', (yyvsp[(1) - (3)].variable), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 44:
#line 424 "src/parser.y"
    {
			(yyval.statement) = ast_statement_assignment((yyvsp[(2) - (3)].ival), TK_ADD_ASG, (yyvsp[(1) - (3)].variable), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 45:
#line 428 "src/parser.y"
    {
			(yyval.statement) = ast_statement_assignment((yyvsp[(2) - (3)].ival), TK_SUB_ASG, (yyvsp[(1) - (3)].variable), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 46:
#line 432 "src/parser.y"
    {
			(yyval.statement) = ast_statement_assignment((yyvsp[(2) - (3)].ival), TK_MUL_ASG, (yyvsp[(1) - (3)].variable), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 47:
#line 436 "src/parser.y"
    {
			(yyval.statement) = ast_statement_assignment((yyvsp[(2) - (3)].ival), TK_DIV_ASG, (yyvsp[(1) - (3)].variable), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 48:
#line 449 "src/parser.y"
    {
			(yyval.variable) = ast_variable_id((yyvsp[(1) - (1)].id));
		;}
    break;

  case 49:
#line 453 "src/parser.y"
    {
			(yyval.variable) = ast_variable_indexed((yyvsp[(2) - (4)].ival), (yyvsp[(1) - (4)].expression), (yyvsp[(3) - (4)].expression));
		;}
    break;

  case 50:
#line 466 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_OR, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 51:
#line 470 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_AND, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 52:
#line 474 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_EQUAL, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 53:
#line 478 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_NEQUAL, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 54:
#line 482 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_LEQUAL, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 55:
#line 486 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), TK_GEQUAL, (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 56:
#line 490 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '<', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 57:
#line 494 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '>', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 58:
#line 498 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '+', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 59:
#line 502 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '-', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 60:
#line 506 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '*', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 61:
#line 510 "src/parser.y"
    {
			(yyval.expression) = ast_expression_binary((yyvsp[(2) - (3)].ival), '/', (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));
		;}
    break;

  case 62:
#line 514 "src/parser.y"
    {
			(yyval.expression) = ast_expression_unary((yyvsp[(1) - (2)].ival), '-', (yyvsp[(2) - (2)].expression));
		;}
    break;

  case 63:
#line 518 "src/parser.y"
    {
			(yyval.expression) = ast_expression_unary((yyvsp[(1) - (2)].ival), TK_NOT, (yyvsp[(2) - (2)].expression));
		;}
    break;

  case 64:
#line 522 "src/parser.y"
    {
			(yyval.expression) = (yyvsp[(1) - (1)].expression);
		;}
    break;

  case 65:
#line 529 "src/parser.y"
    {
			(yyval.expression) = (yyvsp[(1) - (1)].expression);
		;}
    break;

  case 66:
#line 533 "src/parser.y"
    {
			(yyval.expression) = ast_expression_variable((yyvsp[(1) - (1)].variable));
		;}
    break;

  case 67:
#line 537 "src/parser.y"
    {
			(yyval.expression) = ast_expression_function_call((yyvsp[(1) - (1)].function_call));
		;}
    break;

  case 68:
#line 541 "src/parser.y"
    {
			(yyval.expression) = (yyvsp[(2) - (3)].expression);
		;}
    break;

  case 69:
#line 548 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_boolean((yyvsp[(1) - (1)].ival), true);
		;}
    break;

  case 70:
#line 552 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_boolean((yyvsp[(1) - (1)].ival), false);
		;}
    break;

  case 71:
#line 556 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_integer((yyvsp[(1) - (1)].literal).line, (yyvsp[(1) - (1)].literal).ival);
		;}
    break;

  case 72:
#line 560 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_float((yyvsp[(1) - (1)].literal).line, (yyvsp[(1) - (1)].literal).fval);
		;}
    break;

  case 73:
#line 564 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_string((yyvsp[(1) - (1)].literal).line, (yyvsp[(1) - (1)].literal).strval);
		;}
    break;

  case 74:
#line 568 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_array((yyvsp[(1) - (3)].ival), (yyvsp[(2) - (3)].expression), false);
		;}
    break;

  case 75:
#line 572 "src/parser.y"
    {
			(yyval.expression) = ast_expression_literal_array((yyvsp[(2) - (4)].ival), (yyvsp[(3) - (4)].expression), true);
		;}
    break;

  case 76:
#line 585 "src/parser.y"
    {
			(yyval.function_call) = ast_call((yyvsp[(2) - (4)].ival), (yyvsp[(1) - (4)].id), (yyvsp[(3) - (4)].expression));
		;}
    break;

  case 77:
#line 589 "src/parser.y"
    {
			(yyval.function_call) = ast_call_method((yyvsp[(4) - (6)].ival), (yyvsp[(1) - (6)].expression), (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].expression));
		;}
    break;

  case 78:
#line 593 "src/parser.y"
    {
			(yyval.function_call) = ast_call_constructor((yyvsp[(2) - (4)].ival), (yyvsp[(1) - (4)].type), (yyvsp[(3) - (4)].expression));
		;}
    break;

  case 79:
#line 606 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(2) - (3)].definition);
		;}
    break;

  case 80:
#line 613 "src/parser.y"
    {
			(yyval.definition) = NULL;
		;}
    break;

  case 81:
#line 617 "src/parser.y"
    {
			APPEND(Definition, (yyval.definition), (yyvsp[(1) - (2)].definition), (yyvsp[(2) - (2)].definition));
		;}
    break;

  case 82:
#line 624 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (2)].definition);
		;}
    break;

  case 83:
#line 628 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (2)].definition);
		;}
    break;

  case 84:
#line 632 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 85:
#line 636 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 86:
#line 643 "src/parser.y"
    {
			(yyval.definition) = ast_definition_method(true, (yyvsp[(2) - (2)].definition));
		;}
    break;

  case 87:
#line 647 "src/parser.y"
    {
			(yyval.definition) = ast_definition_method(false, (yyvsp[(1) - (1)].definition));
		;}
    break;

  case 88:
#line 654 "src/parser.y"
    {
			(yyval.definition) = ast_definition_constructor((yyvsp[(2) - (3)].definition), (yyvsp[(3) - (3)].block));
		;}
    break;

  case 89:
#line 668 "src/parser.y"
    {
			(yyval.expression) = NULL;
		;}
    break;

  case 90:
#line 672 "src/parser.y"
    {
			(yyval.expression) = (yyvsp[(1) - (1)].expression);
		;}
    break;

  case 91:
#line 680 "src/parser.y"
    {
			(yyval.expression) = (yyvsp[(1) - (1)].expression);
		;}
    break;

  case 92:
#line 684 "src/parser.y"
    {
			Expression* e; // OBS: Can't use APPEND because it's a linked list
			for (e = (yyval.expression) = (yyvsp[(1) - (3)].expression); e->next; e = e->next);
			((yyvsp[(3) - (3)].expression)->previous = e)->next = (yyvsp[(3) - (3)].expression); // linking
		;}
    break;

  case 93:
#line 694 "src/parser.y"
    {
			Variable* variable = ast_variable_id((yyvsp[(1) - (3)].id));
			variable->type = (yyvsp[(3) - (3)].type);
			(yyval.definition) = ast_definition_variable(variable, NULL);
		;}
    break;

  case 94:
#line 703 "src/parser.y"
    {
			(yyval.definition) = ast_definition_variable(ast_variable_id((yyvsp[(1) - (1)].id)), NULL);
		;}
    break;

  case 95:
#line 707 "src/parser.y"
    {
			APPEND(Definition, (yyval.definition), (yyvsp[(1) - (3)].definition),
				ast_definition_variable(ast_variable_id((yyvsp[(3) - (3)].id)), NULL));
		;}
    break;

  case 96:
#line 715 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 97:
#line 726 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (3)].definition);
			Definition* last = NULL;
			for (; (yyvsp[(1) - (3)].definition); last = (yyvsp[(1) - (3)].definition), (yyvsp[(1) - (3)].definition) = (yyvsp[(1) - (3)].definition)->next) {
				(yyvsp[(1) - (3)].definition)->variable.variable->type = (yyvsp[(3) - (3)].definition)->variable.variable->type;
			}
			last->next = (yyvsp[(3) - (3)].definition); // there is always at least one lower_id
		;}
    break;

  case 98:
#line 739 "src/parser.y"
    {
			(yyval.definition) = NULL;
		;}
    break;

  case 99:
#line 743 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(2) - (3)].definition);
		;}
    break;

  case 100:
#line 750 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 101:
#line 754 "src/parser.y"
    {
			APPEND(Definition, (yyval.definition), (yyvsp[(1) - (3)].definition), (yyvsp[(3) - (3)].definition));
		;}
    break;

  case 102:
#line 761 "src/parser.y"
    {
			for ((yyval.definition) = (yyvsp[(1) - (1)].definition); (yyvsp[(1) - (1)].definition); (yyvsp[(1) - (1)].definition) = (yyvsp[(1) - (1)].definition)->next) {
				(yyvsp[(1) - (1)].definition)->variable.variable->value = true;
			}
		;}
    break;

  case 103:
#line 771 "src/parser.y"
    {
			(yyval.definition) = (yyvsp[(1) - (1)].definition);
		;}
    break;

  case 104:
#line 780 "src/parser.y"
    {
			Variable* variable = ast_variable_id((yyvsp[(1) - (3)].id));
			variable->value = true;
			(yyval.definition) = ast_definition_variable(variable, (yyvsp[(3) - (3)].expression));
		;}
    break;


/* Line 1267 of yacc.c.  */
#line 2405 "parser.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 787 "src/parser.y"


static void yyerror(const char* err) {
	parser_error(0, (char*)err); // TODO: Line
}

