#!/bin/bash
make                           && \
clear                          && \
./bin/aria -b test.aria        && \
llvm-dis aria.bc               && \
opt -o aria-opt.bc -O2 aria.ll && \
llvm-dis aria-opt.bc           && \
llc -filetype=obj aria-opt.bc  && \
gcc aria-opt.o -lpthread       && \
./a.out
