#include <stdbool.h>
#include <assert.h>
#include <stddef.h>

#include "arraylist.h"
#include "macros.h"

#define DEFAULT_CAPACITY 16;

struct ArrayList {
	size_t capacity;
	size_t size;

	ArrayListValue* values;
};

// ==================================================
//
//	Auxiliary
//
// ==================================================

static void grow_list(ArrayList* list) {
	// Doubles the capacity
	list->capacity *= 2;
	ArrayListValue* values;
	MALLOC_ARRAY(values, ArrayListValue, list->capacity);

	// Copies values to new array
	for (int i = 0; i < list->size; i++) {
		values[i] = list->values[i];
	}
	free(list->values);
	list->values = values;
}

// ==================================================
//
//	ArrayList functions
//
// ==================================================

ArrayList* al_new(void) {
	ArrayList* list;
	MONGA_MALLOC(list, ArrayList);
	list->capacity = DEFAULT_CAPACITY;
	list->size = 0;
	MALLOC_ARRAY(list->values, ArrayListValue, list->capacity);
	return list;
}

size_t al_capacity(ArrayList* list) {
	assert(list != NULL);
	return list->capacity;
}

size_t al_size(ArrayList* list) {
	assert(list != NULL);
	return list->size;
}

bool al_empty(ArrayList* list) {
	assert(list != NULL);
	return list->size == 0;
}

void al_append(ArrayList* list, ArrayListValue value) {
	assert(list != NULL);
	if (list->size == list->capacity) {
		grow_list(list);
	}
	list->values[list->size++] = value;
}

ArrayListValue al_get(ArrayList* list, ArrayListIndex index) {
	assert(list != NULL);
	assert(index < list->size);
	return list->values[index];
}

void al_replace(ArrayList* list, ArrayListIndex index, ArrayListValue value) {
	assert(list != NULL);
	assert(index < list->size);
	list->values[index] = value;
}

void al_free(ArrayList* list) {
	assert(list != NULL);
	free(list->values);
	free(list);
}
