#if !defined(arraylist_h)
#define arraylist_h

#include <stdbool.h>

typedef struct ArrayList ArrayList;
typedef void* ArrayListValue;
typedef unsigned int ArrayListIndex;

extern ArrayList* al_new(void);

extern size_t al_capacity(ArrayList* list);

extern size_t al_size(ArrayList* list);

extern bool al_empty(ArrayList* list);

// Doubles the capacity of the list if necessary
extern void al_append(ArrayList* list, ArrayListValue value);

extern ArrayListValue al_get(ArrayList* list, ArrayListIndex index);

extern void al_replace(ArrayList* list, ArrayListIndex index,
	ArrayListValue value);

extern void al_free(ArrayList* list);

#endif