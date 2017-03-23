#
# EVA language compiler
#

CC := gcc-5 -std=c99 -Wall

main: objs

objs: scanner

scanner:
	flex src/eva.l
	$(CC) $(CFLAGS) -c lex.yy.c -o obj/scanner.o
