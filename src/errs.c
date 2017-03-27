#include <stdio.h>
#include <stdlib.h>

#include "errs.h"

void scanner_error(ScannerError err) {
	switch (err) {
	case ERR_COMMENT:
		printf("open commentary");
	case ERR_STRING_ESCAPE:
		printf("invalid escape");
	case ERR_STRING_OPEN:
		printf("open string");
	case ERR_STRING_LINE:
		printf("multiline string");
	default:
		printf("scanner unknown error - %d", err);
	}
	printf("\n");
	exit(1);
}

void internal_error(AriaError err) {
	// TODO
}