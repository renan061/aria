#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <time.h>

#define N           10000
#define NTHREADS    100
#define RUNS        10

float **a, *b, *r;

struct timespec start;
struct timespec end;
double mem;
double partials[RUNS];

#define START \
    assert(!clock_gettime(CLOCK_MONOTONIC, &start));
#define END \
    assert(!clock_gettime(CLOCK_MONOTONIC, &end)); \
    partials[i] = diff(start, end);

double diff(struct timespec start, struct timespec end) {
    double diff = (end.tv_sec - start.tv_sec) * 1e9;
    return (diff + (end.tv_nsec - start.tv_nsec)) * 1e-9;
}

void init(void) {
    assert(!clock_gettime(CLOCK_MONOTONIC, &start));
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
    assert(!clock_gettime(CLOCK_MONOTONIC, &end));
    mem = diff(start, end);
}

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

void stats(void) {
    double avg = 0;
    double min = DBL_MAX;
    double max = 0;

    for (int i = 0; i < RUNS; i++) {
        avg += partials[i];
        min = (partials[i] < min) ? partials[i] : min;
        max = (partials[i] > max) ? partials[i] : max;
    }
    avg /= RUNS;

    double diffmax = max - avg;
    double diffmin = avg - min;

    printf("\n");
    printf("\tMem: %f sec\n", mem);
    printf("\n");
    printf("\tAvg: %f sec\n", avg);
    printf("\tMax: %f sec (+%f)\n", max, diffmax);
    printf("\tMin: %f sec (-%f)\n", min, diffmin);
    printf("\t- %f%%\n", diffmin / avg * 100);
    printf("\t+ %f%%\n", diffmax / avg * 100);
    printf("\n");
}
