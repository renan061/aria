#include <stdio.h>

#include "ast_printer.c"

#include "ast.h"
#include "parser.h"
#include "scanner.h"
#include "sem.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("error: missing arguments to main\n");
    	return 1;
    }

	scanner_setup(argv[1]);
	yyparse();
	scanner_clean();
	sem_analyse(program);

    return 0;
}