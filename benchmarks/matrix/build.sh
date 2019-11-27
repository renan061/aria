#!/bin/bash
lua script.lua                         && \
cp ../../bin/aria aria                 && \
./aria -b test.aria                    && \
llvm-dis aria.bc                       && \
opt -time-passes -o opt.bc -O2 aria.ll && \
llvm-dis opt.bc                        && \
llc -filetype=obj opt.bc               && \
gcc opt.o -lpthread
