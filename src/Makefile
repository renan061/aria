#
# Makefile for building Aria
#

RM= rm -f

BIN= ../bin/
ARIA= aria
TO_BIN= $(BIN)/$(ARIA)

FLEX= flex
FLEX_FLAGS= -o scanner.c
BISON= bison
BISON_FLAGS= -v --defines=bison.h -o bison.c

# LLVM settings.
LLVM_INCLUDEDIR= -I$(shell llvm-config --includedir)
LLVM_CXXFLAGS= $(shell llvm-config --cxxflags)
LLVM_LDFLAGS= $(shell llvm-config --ldflags)
LLVM_LIBS= $(shell llvm-config --libs) $(shell llvm-config --system-libs)

# Compiler settings.
CC= gcc
CFLAGS= -O2 -Wall -Werror -std=gnu11 $(LLVM_INCLUDEDIR)

CXXC= clang++
CXXFLAGS= $(LLVM_CXXFLAGS)

LDFLAGS= $(LLVM_LDFLAGS) $(LLVM_LIBS)

# Objects.
OBJS= scanner.o bison.o ast.o sem.o backend.o aria.o \
	errs.o vector.o symtable.o ir.o athreads.o

# Targets start here.
default: tools $(OBJS)
	$(CXXC) $(CXXFLAGS) -o $(TO_BIN) $(OBJS) $(LDFLAGS)

tools:
	$(FLEX) $(FLEX_FLAGS) scanner.l
	$(BISON) $(BISON_FLAGS) parser.y

clean:
	$(RM) scanner.c
	$(RM) bison.*
	$(RM) *.o

# Binary dependencies.
scanner.o: scanner.c
bison.o: bison.c bison.h
ast.o: ast.c ast.h
sem.o: sem.c sem.h
backend.o: backend.c backend.h
aria.o: aria.c
# ---
errs.o: errs.c errs.h
vector.o: vector.c vector.h
symtable.o: symtable.c symtable.h
ir.o: ir.c ir.h
athreads.o: athreads.c athreads.h
