#!/bin/bash
./bin/aria -b test.aria
llvm-dis aria.bc
opt -o aria-opt.bc -O2 aria.ll
llvm-dis aria-opt.bc
