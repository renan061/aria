#if !defined(scanner_h)
#define scanner_h

// the token type to be used in other modules
typedef unsigned int Token;

// used by the parser to get the next token
extern int yylex(void);

// TODO
typedef enum ScannerNativeIndex {
    SCANNER_NATIVE_BOOLEAN = 0,
    SCANNER_NATIVE_CONDITION_QUEUE,
    SCANNER_NATIVE_FLOAT,
    SCANNER_NATIVE_INTEGER,
    SCANNER_NATIVE_MAIN,
    SCANNER_NATIVE_SELF,
    SCANNER_NATIVE_STRING,
    SCANNER_NATIVE_UNLOCKED
} ScannerNativeIndex;

#define SCANNER_NATIVE_SIZE 8

#define TK_LEARROW TK_LEQUAL

extern const char* scanner_native[SCANNER_NATIVE_SIZE];

// <setup> and <clean> should be called before starting to use the
// scanner and after finishing doing so, respectively
extern void scanner_setup(const char* file);
extern void scanner_clean(void);

#endif
