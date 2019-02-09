#if !defined(scanner_h)
#define scanner_h

// The token type to be used in other modules.
typedef unsigned int Token;

// Used by the parser to get the next token.
extern int yylex(void);

// TODO
typedef enum ScannerNativeIndex {
    SCANNER_NATIVE_BOOLEAN = 0,
    SCANNER_NATIVE_INTEGER,
    SCANNER_NATIVE_FLOAT,
    SCANNER_NATIVE_STRING,
    SCANNER_NATIVE_CONDITION_QUEUE,
    SCANNER_NATIVE_SELF,
    SCANNER_NATIVE_MAIN
} ScannerNativeIndex;

#define SCANNER_NATIVE_SIZE 7

extern const char* scanner_native[SCANNER_NATIVE_SIZE];

// Setup and Clean should be called before starting
// to use the scanner and after finishing doing so,
// respectively.
extern void scanner_setup(const char* file);
extern void scanner_clean(void);

#endif
