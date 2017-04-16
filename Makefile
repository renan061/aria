#
# ARIA language compiler
#

CC := gcc-5 -std=c11 -Wall

main: objs

objs: errs vector scanner parser ast

# ==================================================
# 
#	Modules
# 
# ==================================================

errs:
	@- $(CC) $(CFLAGS) -c src/errs.c -o obj/errs.o

vector: errs
	@- $(CC) $(CFLAGS) -c src/vector.c -o obj/vector.o

scanner: errs vector parser
	@- flex src/scanner.l
	@- mv lex.yy.c src/lex.c
	@- $(CC) $(CFLAGS) -c src/lex.c -o obj/scanner.o -Isrc/

parser: errs ast
	@- bison -v --defines=src/bison.h src/parser.y
	@- mv parser.tab.c src/bison.c
	@- mkdir -p temp
	@- mv parser.output temp/bison.output
	@- $(CC) $(CFLAGS) -c src/bison.c -o obj/parser.o

ast:
	@- $(CC) $(CFLAGS) -c src/ast.c -o obj/ast.o

# ==================================================
# 
#	Tests
# 
# ==================================================

vector_test: vector
	@- $(CC) $(CFLAGS) -o bin/vectortest	\
	obj/errs.o obj/vector.o					\
	src/vector_test.c
	
	@- ./bin/vectortest

scanner_test: scanner
	@- $(CC) $(CFLAGS) -o bin/scannertest							\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	src/scanner_test.c

	@- sh tests/test.sh scanner

parser_test: scanner parser ast
	@- $(CC) $(CFLAGS) -o bin/parsertest							\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	src/parser_test.c

	@- sh tests/test.sh parser

test: clean vector_test scanner_test parser_test

# ==================================================
# 
#	Misc
# 
# ==================================================

clean:
	@- rm -f src/lex.c
	@- rm -f src/bison.c
	@- rm -f src/bison.h
	@- rm -rf temp
	@- rm -f obj/*.o
	@- rm -f bin/*
