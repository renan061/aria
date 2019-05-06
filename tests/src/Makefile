#
# Makefile for testing Aria
#

CP= cp
RM= rm -f

SRC= ../../src/
BIN= ../bin/

# LLVM settings.
LLVM_INCLUDEDIR= -I$(shell llvm-config --includedir)

# Compiler settings.
CC= gcc
CFLAGS= -O2 -Wall -Werror -std=gnu11 $(LLVM_INCLUDEDIR) -I$(SRC)

# Objects.
OBJS= vector_test.o scanner_test.o parser_test.o ast_test.o sem_test.o

VECTOR_OBJS= errs.o vector.o
SCANNER_OBJS= $(VECTOR_OBJS) scanner.o bison.o ast.o
PARSER_OBJS= $(SCANNER_OBJS)
AST_OBJS= $(SCANNER_OBJS)
SEM_OBJS= $(AST_OBJS) symtable.o sem.o

# Targets start here.
build: $(OBJS)
	$(CP) $(SRC)*.o ./
	$(CC) $(CFLAGS) -o $(BIN)vectortest $(VECTOR_OBJS) vector_test.c -I$(SRC)
	$(CC) $(CFLAGS) -o $(BIN)scannertest $(SCANNER_OBJS) scanner_test.c -I$(SRC)
	$(CC) $(CFLAGS) -o $(BIN)parsertest $(PARSER_OBJS) parser_test.c -I$(SRC)
	$(CC) $(CFLAGS) -o $(BIN)asttest $(AST_OBJS) ast_test.c -I$(SRC)
	$(CC) $(CFLAGS) -o $(BIN)semtest $(SEM_OBJS) sem_test.c -I$(SRC)

clean:
	$(RM) *.o

# Binary dependencies.
vector_test.o: vector_test.c
scanner_test.o: scanner_test.c
parser_test.o: parser_test.c
ast_test.o: ast_test.c
sem_test.o: sem_test.c
