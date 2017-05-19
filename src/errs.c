#include <stdio.h>
#include <stdlib.h>

// TODO: `aria:`

void internal_error(const char* err) {
	fprintf(stderr, "internal error: %s\n", err);
	exit(1);
}

void memory_error(const char* err) {
	fprintf(stderr, "memory error: %s\n", err);
	exit(1);
}

void scanner_error(unsigned int line, const char* err) {
	fprintf(stderr, "line %d:\n\tscanner error: %s\n", line, err);
	exit(1);
}

void parser_error(unsigned int line, const char* err) {
	fprintf(stderr, "line %d:\n\tparser error: %s\n", line, err);
	exit(1);
}

void sem_error(unsigned int line, const char* err) {
	fprintf(stderr, "line %d:\n\tsemantic error: %s\n", line, err);
	exit(1);
}
