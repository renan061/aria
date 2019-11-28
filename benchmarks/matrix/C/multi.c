#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>

#define NTHREADS 8
#define N 50000

int size = N / NTHREADS;
float **a, *b, *r;

void printarray(float array[N]) {
    printf("[");
    for (int i = 0;;) {
        printf("%.2f", array[i]);
        if (++i == N) {
            break;
        } else {
            printf(", ");
        }
    }
    printf("]\n");
}

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
    srand(time(NULL));
    a = (float**)malloc(N * sizeof(float*));
    b = (float*)malloc(N * sizeof(float));
    r = (float*)malloc(N * sizeof(float));
    for (int i = 0; i < N; i++) {
        a[i] = (float*)malloc(N * sizeof(float));
        b[i] = (rand() % 1000) / 10.0;
        for (int j = 0; j < N; j++) {
            a[i][j] = (rand() % 1000) / 10.0;
        }
    }

    pthread_t threads[NTHREADS];
    int ranges[NTHREADS];

    for (int i = 0; i < NTHREADS; i++) {
        ranges[i] = i * size;
        pthread_create(&threads[i], NULL, mul, &ranges[i]);
    }

    for (int i = 0; i < NTHREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    return 0;
}
