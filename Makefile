#
# EVA language compiler
#

CC := gcc-5 -std=c99 -Wall

main: objs

objs: scanner

scanner:
	flex src/eva.l
	$(CC) $(CFLAGS) -c lex.yy.c -o obj/scanner.o -Isrc/

scanner_test: scanner
	$(CC) $(CFLAGS) -o bin/scannertest obj/scanner.o src/scanner_test.c
	sh tests/test.sh scanner
	# ./bin/scannertest test.eva

clean:
	$(RM) lex.yy.c
