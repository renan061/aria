#
# Makefile
#

RM= rm -f
CC= gcc
CFLAGS= -O2 -Wall -Werror -std=gnu11

build:
	$(CC) $(CFLAGS) single.c -o single.out
	$(CC) $(CFLAGS) multi.c  -o multi.out -pthread

clean:
	$(RM) *.o
	$(RM) *.out
