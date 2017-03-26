#include <stdio.h>
#include "scanner.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
    	printf("todo error shell\n");
    	return 1;
    }

    if (!(yyin = fopen(argv[1], "r"))) {
    	printf("todo error input file\n");   
    	return 1;
    }

	scanner_setup();

	int token;
	do {
		token = yylex();
		switch (token) {
		case TK_FUNCTION:		printf("TK_FUNCTION");		break;
		case TK_SHORT_ASG:		printf("TK_SHORT_ASG");		break;
		case TK_WHILE:			printf("TK_WHILE");			break;
		case TK_WAIT:			printf("TK_WAIT");			break;
		case TK_IN:				printf("TK_IN");			break;
		case TK_SIGNAL:			printf("TK_SIGNAL");		break;
		case TK_BROADCAST:		printf("TK_BROADCAST");		break;
		case TK_RETURN:			printf("TK_RETURN");		break;
		case TK_IF:				printf("TK_IF");			break;
		case TK_ELSE:			printf("TK_ELSE");			break;
		case TK_FOR:			printf("TK_FOR");			break;
		case TK_SPAWN:			printf("TK_SPAWN");			break;
		case TK_OR:				printf("TK_OR");			break;
		case TK_AND:			printf("TK_AND");			break;
		case TK_EQUAL:			printf("TK_EQUAL");			break;
		case TK_LEQUAL:			printf("TK_LEQUAL");		break;
		case TK_GEQUAL:			printf("TK_GEQUAL");		break;
		case TK_NOT:			printf("TK_NOT");			break;
		case TK_TRUE:			printf("TK_TRUE");			break;
		case TK_FALSE:			printf("TK_FALSE");			break;
		case TK_MONITOR:		printf("TK_MONITOR");		break;
		case TK_PRIVATE:		printf("TK_PRIVATE");		break;
		case TK_INITIALIZER:	printf("TK_INITIALIZER");	break;
		case TK_LOWER_ID:
			printf("TK_LOWER_ID - %s", yylval.u.strval);
			break;
		case TK_UPPER_ID:
			printf("TK_UPPER_ID - %s", yylval.u.strval);
			break;
		case TK_INTEGER:
			printf("TK_INTEGER - %d", yylval.u.ival);
			break;
		case TK_FLOAT:
			printf("TK_FLOAT - %f", yylval.u.fval);
			break;
		case TK_STRING:
			printf("TK_STRING - %s", yylval.u.strval);
			break;
		case 0:
			goto WHILE_END;
		default:
			printf("%c", token);
		}
		printf("\n");
	WHILE_END:
		;
	} while(token != 0);

	scanner_clean();
    return 0;
}
