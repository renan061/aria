#
# Makefile
#

RM= rm -f
ARIA= ../../bin/aria

run:
	lua addvalues.lua
	$(ARIA) -b test.aria
	llvm-dis aria.bc
	opt -o opt.bc -O2 aria.ll
	llvm-dis opt.bc
	llc -filetype=obj opt.bc
	gcc opt.o -lpthread
	perf stat -r 1000 ./a.out

clean:
	$(RM) *.ll
	$(RM) *.bc
