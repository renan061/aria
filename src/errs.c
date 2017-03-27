#include <stdio.h>
#include <stdlib.h>

extern void scanner_error(unsigned int line, char* err) {
	fprintf(stderr, "line %d:\n\tscanner error: %s\n", line, err);
	exit(1);
}

extern void internal_error(char* err) {
	fprintf(stderr, "internal error: %s\n", err);
	exit(1);
}
