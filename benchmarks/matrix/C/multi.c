#include <pthread.h>

#include "common.c"

int size = N / NTHREADS;

void* mul(void* param) {
    int range = *(int*)(param);
    for (int i = range; i < size; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += a[i][j] * b[j];
        }
    }
    return NULL;
}

int main(void) {
    init();
    for (int i = 0; i < RUNS; i++) {
        START;
        pthread_t threads[NTHREADS];
        int ranges[NTHREADS];
        for (int i = 0; i < NTHREADS; i++) {
            ranges[i] = i * size;
            pthread_create(&threads[i], NULL, mul, &ranges[i]);
        }
        for (int i = 0; i < NTHREADS; i++) {
            pthread_join(threads[i], NULL);
        }
        END;
    }
    stats();
    return 0;
}
