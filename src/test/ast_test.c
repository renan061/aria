#include <stdio.h>

#include "ast_printer.c"

#include "ast.h"
#include "parser.h"
#include "scanner.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("error: missing arguments to main\n");
    	return 1;
    }

	scanner_setup(argv[1]);
	yyparse();
	scanner_clean();
	print_ast_program(program);

    return 0;
}