#include <stdio.h>

#include "parser.h"
#include "scanner.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("error: missing arguments to main\n");
    	return 1;
    }
	scanner_setup(argv[1]);

	if (!yyparse()) {
		printf("OK");
	}
	
	scanner_clean();
    return 0;
}
