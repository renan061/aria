#if !defined(errs_h)
#define errs_h

extern void scanner_error(unsigned int line, char* err);
extern void internal_error(char* err);

#endif
