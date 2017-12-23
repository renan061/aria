#if !defined(errs_h)
#define errs_h

#define UNREACHABLE assert(NULL)

extern void internal_error(char* const err);
extern void memory_error(char* const err);
extern void scanner_error(unsigned int line, const char* err);
extern void parser_error(unsigned int line, const char* err);
extern void sem_error(unsigned int line, const char* err);

#endif
