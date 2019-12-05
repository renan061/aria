#include <assert.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <time.h>

#define N           10000
#define NTHREADS    8
#define RUNS        10

#define SINGLE 0
#define MULTI  1

#define START   double start = gettime();
#define END     partials[now][i]  = gettime() - start;

int now; // SINGLE or NOW
float **a, *b, *r;
double partials[2][RUNS];
int size = N / NTHREADS;

double gettime(void);
void init(void);
void stats(void);
void printarray(float array[N]);

void* mul_multi(void* param) {
    int range = *(int*)(param);
    int limit = range + size;
    for (int i = range; i < limit; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += a[i][j] * b[j];
        }
    }
    return NULL;
}

void mul_single(void) {
    for (int i = 0; i < N; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += a[i][j] * b[j];
        }
    }
}

int main(void) {
    init();

    now = SINGLE;
    for (int i = 0; i < RUNS; i++) {
        START;
        mul_single();
        END;
    }

    now = MULTI;
    for (int i = 0; i < RUNS; i++) {
        START;
        pthread_t threads[NTHREADS];
        int ranges[NTHREADS];
        for (int i = 0; i < NTHREADS; i++) {
            ranges[i] = i * size;
            pthread_create(&threads[i], NULL, mul_multi, &ranges[i]);
        }
        for (int i = 0; i < NTHREADS; i++) {
            pthread_join(threads[i], NULL);
        }
        END;
    }

    stats();
    return 0;
}

// -----------------------------------------------------------------------------

double gettime(void) {
    struct timespec ts;
    assert(!clock_gettime(CLOCK_MONOTONIC, &ts)); // CLOCK_REALTIME
    return ((double)(ts.tv_sec) + ((double)(ts.tv_nsec) / 1e9));
}

void init(void) {
    double start = gettime();
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
    double mem = gettime() - start;

    printf("\n");
    printf("\tMem: %f sec\n", mem);
    printf("\n");
}

void stats(void) {
    double avg[2] = {0, 0};
    double min[2] = {DBL_MAX, DBL_MAX};
    double max[2] = {0, 0};
    double x;

    printf("\tRuns: %d\n\n", RUNS);

    for (now = 0; now < 2; now++) {
        for (int i = 0; i < RUNS; i++) {
            x = partials[now][i];
            avg[now] += x;
            min[now] = (x < min[now]) ? x : min[now];
            max[now] = (x > max[now]) ? x : max[now];
        }

        avg[now] /= RUNS;

        double diffmax = max[now] - avg[now];
        double diffmin = avg[now] - min[now];

        printf("\t");
        if (now == SINGLE) {
            printf("SINGLE");
        } else {
            printf("MULTI (%d threads)", NTHREADS);
        }
        printf("\n");
        printf("\tAvg: %f sec\n", avg[now]);
        printf("\tMax: %f sec (+%f)\n", max[now], diffmax);
        printf("\tMin: %f sec (-%f)\n", min[now], diffmin);
        printf("\t- %f%%\n", diffmin / avg[now] * 100);
        printf("\t+ %f%%\n", diffmax / avg[now] * 100);
        printf("\n");
    }

    double speedup = avg[SINGLE] / avg[MULTI];
    printf("\tSpeedup: %.2f\n", speedup);
    printf("\tThread Efficiency: %.2f%%\n", speedup / NTHREADS * 100);
    printf("\n");
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
