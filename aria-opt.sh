#!/bin/bash
make
clear
./bin/aria -r test.aria
llvm-dis aria.bc
opt -o aria-opt.bc -O2 aria.ll
llvm-dis aria-opt.bc
