#
# ARIA language compiler
#

# TODO: gnu11 / c11
CC := gcc-5 -std=gnu11 -Wall

main: objs

objs: errs vector scanner parser ast sem

# ==================================================
# 
#	Modules
# 
# ==================================================

errs:
	@- $(CC) $(CFLAGS) -c src/errs.c -o obj/errs.o

vector:
	@- $(CC) $(CFLAGS) -c src/vector.c -o obj/vector.o

scanner:
	@- flex src/scanner.l
	@- mv lex.yy.c src/lex.c
	@- $(CC) $(CFLAGS) -c src/lex.c -o obj/scanner.o -Isrc/

parser:
	@- bison -v --defines=src/bison.h src/parser.y
	@- mv parser.tab.c src/bison.c
	@- mkdir -p temp
	@- mv parser.output temp/bison.output
	@- $(CC) $(CFLAGS) -c src/bison.c -o obj/parser.o

ast:
	@- $(CC) $(CFLAGS) -c src/ast.c -o obj/ast.o

sem:
	@- $(CC) $(CFLAGS) -c src/symtable.c -o obj/symtable.o
	@- $(CC) $(CFLAGS) -c src/sem.c -o obj/sem.o

# ==================================================
# 
#	Tests
# 
# ==================================================

vector_test: errs vector
	@- $(CC) $(CFLAGS) -o bin/vectortest	\
	obj/errs.o obj/vector.o					\
	src/test/vector_test.c -Isrc/
	
	@- ./bin/vectortest

scanner_test: errs vector parser scanner ast
	@- $(CC) $(CFLAGS) -o bin/scannertest							\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	src/test/scanner_test.c -Isrc/

	@- sh tests/test.sh scanner

parser_test: errs vector parser scanner ast
	@- $(CC) $(CFLAGS) -o bin/parsertest							\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	src/test/parser_test.c -Isrc/

	@- sh tests/test.sh parser

ast_test: errs vector parser scanner ast
	@- $(CC) $(CFLAGS) -o bin/asttest								\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	src/test/ast_test.c -Isrc/

	@- sh tests/test.sh ast

sem_test: errs vector parser scanner ast sem
	@- $(CC) $(CFLAGS) -o bin/semtest								\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o obj/ast.o	\
	obj/symtable.o obj/sem.o										\
	src/test/sem_test.c -Isrc/

	@- sh tests/test.sh sem

test: clean vector_test scanner_test parser_test ast_test sem_test

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
