/*
 * TODO: Remove
 */
#if !defined(macros_h)
#define macros_h

// TODO: Necessary?
#include <stdio.h>
#include <stdlib.h>

#define MONGA_ERR(...) fprintf(stderr, __VA_ARGS__); exit(1);

#define MONGA_INTERNAL_ERR(...)				\
 	fprintf(stderr, "internal error: ");	\
 	MONGA_ERR(__VA_ARGS__);					\

// TODO: Rename to MALLOC
#define MONGA_MALLOC(n, type)						\
	n = (type*)malloc(sizeof(type));				\
	if (n == NULL) {								\
		MONGA_ERR("error: insufficient memory\n");	\
	}												\

#define MALLOC_ARRAY(n, elem_type, len)					\
	n = (elem_type*)malloc(len * sizeof(elem_type));	\
	if (n == NULL) {									\
		MONGA_ERR("error: insufficient memory\n");		\
	}													\

#endif