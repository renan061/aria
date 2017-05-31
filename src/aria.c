#include <stdio.h> // TODO: Remove

#include "ast.h"
#include "parser.h"
#include "scanner.h"
#include "sem.h"
#include "backend.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("error: missing arguments to main\n");
    	return 1;
    }

	scanner_setup(argv[1]);
	yyparse();
	scanner_clean();
	sem_analyse(program);
	backend_compile(program);

	return 0;
}