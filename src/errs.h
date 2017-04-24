#if !defined(errs_h)
#define errs_h

extern void internal_error(char* err);
extern void memory_error(char* err);
extern void scanner_error(unsigned int line, char* err);
extern void parser_error(unsigned int line, char* err);

#endif
