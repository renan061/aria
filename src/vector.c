#include <stddef.h>

#include "alloc.h"
#include "vector.h"

#define DEFAULT_CAPACITY 16;

struct Vector {
	size_t capacity;
	size_t size;
	void** values;
};

Vector* vector_new(void) {
	Vector* vector;
	MALLOC(vector, Vector);
	vector->capacity = DEFAULT_CAPACITY;
	vector->size = 0;
	MALLOC_ARRAY(vector->values, void*, vector->capacity);
	return vector;
}

size_t vector_capacity(Vector* vector) {
	return vector->capacity;
}

size_t vector_size(Vector* vector) {
	return vector->size;
}

void* vector_get(Vector* vector, unsigned int index) {
	return vector->values[index];
}

void vector_append(Vector* vector, void* value) {
	if (vector->size == vector->capacity) {
		vector->capacity *= 2;
		REALLOC(vector->values, void*, vector->capacity);
	}
	vector->values[vector->size++] = value;
}

void vector_destroy(Vector* vector) {
	free(vector->values);
	free(vector);
}
