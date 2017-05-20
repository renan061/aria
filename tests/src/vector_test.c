#include <assert.h>
#include <stdio.h>

#include "vector.h"

char* vals[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
	"13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"};

int main(void) {
	// vector_new
	Vector* vector = vector_new();
	assert(vector);

	// vector_size
	assert(vector_size(vector) == 0);

	// vector_append
	for (int i = 0; i < 24; i++) {
		assert(vector_size(vector) == i);
		vector_append(vector, vals[i]);
	}

	// vector_get
	for (int i = 0; i < 24; i++) {
		assert(vector_get(vector, i) == vals[i]);
	}

	// vector_destroy
	vector_destroy(vector);

	printf("OK vector test\n");
	return 0;
}