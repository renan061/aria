#if !defined(errs_h)
#define errs_h

typedef enum AriaError {
	ERROR_INSUFFICIENT_MEMORY, // TODO: Is this an internal error?
} AriaError;

typedef enum ScannerError {
	ERR_COMMENT,
	ERR_STRING_ESCAPE,
	ERR_STRING_OPEN,
	ERR_STRING_LINE,
} ScannerError;

extern void scanner_error(ScannerError err);

extern void internal_error(AriaError err);

#endif
