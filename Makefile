#
# ARIA language compiler
#

CC := gcc-5 -std=c99 -Wall

main: objs

objs: errs vector scanner

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
	@- flex src/aria.l
	@- mv lex.yy.c src/lex.c
	@- $(CC) $(CFLAGS) -c src/lex.c -o obj/scanner.o -Isrc/

parser: errs
	@- bison -v -d src/aria.y
	@- mv aria.tab.c src/bison.c
	@- mv aria.tab.h src/bison.h
	@- mkdir -p temp
	@- mv aria.output temp/bison.output
	@- $(CC) $(CFLAGS) -c src/bison.c -o obj/parser.o

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
	@- $(CC) $(CFLAGS) -o bin/scannertest				\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o	\
	src/scanner_test.c

	@- sh tests/test.sh scanner

parser_test: scanner parser
	@- $(CC) $(CFLAGS) -o bin/parsertest				\
	obj/errs.o obj/vector.o obj/scanner.o obj/parser.o	\
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
