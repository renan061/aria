#if !defined(alloc_h)
#define alloc_h

#include <stdlib.h>
#include "errs.h"

#define ERROR_MEMORY "not enough memory"

#define ERROR memory_error(ERROR_MEMORY)

#define MALLOC(x, t)				\
	x = (t*)malloc(sizeof(t));		\
	if (!x) ERROR;					\

#define MALLOC_ARRAY(x, t, n)		\
	x = (t*)malloc(n * sizeof(t));	\
	if (!x) ERROR;					\

#define REALLOC(x, t, n)				\
	x = (t*)realloc(x, n * sizeof(t));	\
	if (!x) ERROR;						\

#endif
