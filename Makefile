#
# EVA language compiler
#

CC := gcc-5 -std=c99 -Wall

main: objs

objs: errs arraylist scanner

errs:
	@- $(CC) $(CFLAGS) -c src/errs.c -o obj/errs.o

arraylist:
	@- $(CC) $(CFLAGS) -c src/arraylist.c -o obj/arraylist.o

scanner: errs arraylist
	@- flex src/eva.l
	@- $(CC) $(CFLAGS) -c lex.yy.c -o obj/scanner.o -Isrc/

scanner_test: scanner
	@- $(CC) $(CFLAGS) -o bin/scannertest \
	obj/errs.o obj/arraylist.o obj/scanner.o src/scanner_test.c

	@- sh tests/test.sh scanner

test: clean scanner_test

clean:
	@- $(RM) lex.yy.c
	@- $(RM) obj/*.o
	@- $(RM) bin/*
