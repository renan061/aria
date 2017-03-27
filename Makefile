#
# EVA language compiler
#

CC := gcc-5 -std=c99 -Wall

main: objs

objs: errs vector scanner

errs:
	@- $(CC) $(CFLAGS) -c src/errs.c -o obj/errs.o

vector: errs
	@- $(CC) $(CFLAGS) -c src/vector.c -o obj/vector.o

scanner: errs vector
	@- flex src/eva.l
	@- $(CC) $(CFLAGS) -c lex.yy.c -o obj/scanner.o -Isrc/

vector_test: vector
	@- $(CC) $(CFLAGS) -o bin/vectortest	\
	obj/errs.o obj/vector.o					\
	src/vector_test.c
	
	@- ./bin/vectortest

scanner_test: scanner
	@- $(CC) $(CFLAGS) -o bin/scannertest	\
	obj/errs.o obj/vector.o obj/scanner.o	\
	src/scanner_test.c

	@- sh tests/test.sh scanner

test: clean vector_test scanner_test

clean:
	@- $(RM) lex.yy.c
	@- $(RM) obj/*.o
	@- $(RM) bin/*
