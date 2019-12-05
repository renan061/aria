#
# Makefile
#

RM= rm -f
CC= gcc
CFLAGS= -O2 -Wall -Werror -std=gnu11

build:
	$(CC) $(CFLAGS) main.c -o main.out -pthread

clean:
	$(RM) *.o
	$(RM) *.out
