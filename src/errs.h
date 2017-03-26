#if !defined(errs_h)
#define errs_h

typedef enum ScannerError {
	ERR_COMMENT,
	ERR_STRING_ESCAPE,
	ERR_STRING_OPEN,
	ERR_STRING_LINE,
} ScannerError;

extern void eva_scanner_error(ScannerError err);

#endif
