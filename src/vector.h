#if !defined(vector_h)
#define vector_h

typedef struct Vector Vector;

Vector* vector_new(void);

size_t vector_capacity(Vector* vector);
size_t vector_size(Vector* vector);

void* vector_get(Vector* vector, unsigned int index);

void vector_append(Vector* vector, void* value);

void vector_destroy(Vector* vector);

#endif