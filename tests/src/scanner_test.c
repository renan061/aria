#include <stdio.h>
#include <strings.h>

#include "parser.h"
#include "scanner.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("error: missing arguments to main\n");
        return 1;
    }
    scanner_setup(argv[1]);
    
    int token;
    do {
        token = yylex();
        switch (token) {
        // operators
        case TK_EQUAL:          printf("TK_EQUAL");         break;
        case TK_NEQUAL:         printf("TK_NEQUAL");        break;
        case TK_LEQUAL:         printf("TK_LEQUAL");        break;
        case TK_GEQUAL:         printf("TK_GEQUAL");        break;
        case TK_NOT:            printf("TK_NOT");           break;
        case TK_AND:            printf("TK_AND");           break;
        case TK_OR:             printf("TK_OR");            break;
        case TK_DEF_ASG:        printf("TK_DEF_ASG");       break;
        case TK_ADD_ASG:        printf("TK_ADD_ASG");       break;
        case TK_SUB_ASG:        printf("TK_SUB_ASG");       break;
        case TK_MUL_ASG:        printf("TK_MUL_ASG");       break;
        case TK_DIV_ASG:        printf("TK_DIV_ASG");       break;
        // keywords
        case TK_BROADCAST:      printf("TK_BROADCAST");     break;
        case TK_ELSE:           printf("TK_ELSE");          break;
        case TK_FALSE:          printf("TK_FALSE");         break;
        case TK_FOR:            printf("TK_FOR");           break;
        case TK_FUNCTION:       printf("TK_FUNCTION");      break;
        case TK_IF:             printf("TK_IF");            break;
        case TK_IMMUTABLE:      printf("TK_IMMUTABLE");     break;
        case TK_IN:             printf("TK_IN");            break;
        case TK_INITIALIZER:    printf("TK_INITIALIZER");   break;
        case TK_MONITOR:        printf("TK_MONITOR");       break;
        case TK_PRIVATE:        printf("TK_PRIVATE");       break;
        case TK_RETURN:         printf("TK_RETURN");        break;
        case TK_SELF:           printf("TK_SELF");          break;
        case TK_SIGNAL:         printf("TK_SIGNAL");        break;
        case TK_SPAWN:          printf("TK_SPAWN");         break;
        case TK_STRUCTURE:      printf("TK_STRUCTURE");     break;
        case TK_TRUE:           printf("TK_TRUE");          break;
        case TK_VALUE:          printf("TK_VALUE");         break;
        case TK_VARIABLE:       printf("TK_VARIABLE");      break;
        case TK_WAIT:           printf("TK_WAIT");          break;
        case TK_WHILE:          printf("TK_WHILE");         break;
        // with values
        case TK_LOWER_ID:
            printf("TK_LOWER_ID - %s", yylval.id->name);
            break;
        case TK_UPPER_ID:
            printf("TK_UPPER_ID - %s", yylval.id->name);
            break;
        case TK_INTEGER:
            printf("TK_INTEGER - %d", yylval.literal.ival);
            break;
        case TK_FLOAT:
            printf("TK_FLOAT - %f", yylval.literal.fval);
            break;
        case TK_STRING:
            printf("TK_STRING - ");
            for (int i = 0; i < strlen(yylval.literal.strval); i++) {
                switch (yylval.literal.strval[i]) {
                case '\n':
                    printf("\\n");
                    break;
                case '\t':
                    printf("\\t");
                    break;
                default:
                    printf("%c", yylval.literal.strval[i]);
                }
            }
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
