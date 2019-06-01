#if !defined(alloc_h)
#define alloc_h

#include <stdlib.h>
#include "errs.h"

#define ERROR_MEMORY "not enough memory"

// TODO: what?
#define ERROR memory_error(ERROR_MEMORY)

#define MALLOC(x, t) do { \
    x = (t*)malloc(sizeof(t)); \
    if (!x) ERROR; \
} while (0) \

#define MALLOC_ARRAY(x, t, n) do { \
    x = (t*)malloc(n * sizeof(t)); \
    if (!x) ERROR; \
} while (0) \

#define REALLOC(x, t, n) do { \
    x = (t*)realloc(x, n * sizeof(t)); \
    if (!x) ERROR; \
} while (0) \

#endif
