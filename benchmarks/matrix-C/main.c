#include <assert.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <time.h>

// configuration
#define N           10000
#define NTHREADS    8
#define RUNS        10
#define SUBRUNS     10

#define SINGLE 0
#define MULTI  1

#define START   double start = gettime();
#define END     partials[now][i] = gettime() - start;

// -----------------------------------------------------------------------------

int now; // SINGLE or NOW
double *a, *b, *c[2];
double partials[2][RUNS];
int section = N / NTHREADS;

double gettime(void);
void init(void);
void stats(void);
void printarray(float array[N]);

void* mul_multi(void* param) {
    int thread = *(int*)(param);
    int start = thread * section;
    int end = start + section;
    int ioff;
    double x;

    for (int i = start; i < end; i++) {
        x = 0;
        ioff = i * N;
        for (int j = 0; j < N; j++) {
            x += a[ioff + j] * b[j];
        }
        c[SINGLE][i] = x;
    }
    return NULL;
}

void mul_single(void) {
    int ioff;
    double x;
    for (int i = 0; i < N; i++) {
        x = 0;
        ioff = i * N;
        for (int j = 0; j < N; j++) {
            x += a[ioff + j] * b[j];
        }
        c[MULTI][i] = x;
    }
}

int main(void) {
    init();

    now = SINGLE;
    for (int i = 0; i < RUNS; i++) {
        START;
        for (int j = 0; j < SUBRUNS; j++) {
            mul_single();
        }
        END;
    }

    now = MULTI;
    for (int i = 0; i < RUNS; i++) {
        START;
        for (int j = 0; j < SUBRUNS; j++) {
            pthread_t threads[NTHREADS];
            int ids[NTHREADS];
            for (int i = 0; i < NTHREADS; i++) {
                ids[i] = i;
                pthread_create(&threads[i], NULL, mul_multi, &ids[i]);
            }
            for (int i = 0; i < NTHREADS; i++) {
                pthread_join(threads[i], NULL);
            }
        }
        END;
    }

    for (int i = 0; i < N; i++) {
        assert(c[SINGLE][i] == c[MULTI][i]);
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
    double t = gettime();
    srand(time(0));
    a = (double*)malloc(N * N * sizeof(double));
    b = (double*)malloc(N *     sizeof(double));
    c[SINGLE] = (double*)malloc(N * sizeof(double));
    c[MULTI]  = (double*)malloc(N * sizeof(double));
    for (int i = 0; i < N; i++) {
        b[i] = rand() / 1000000;
        for (int j = 0; j < N; j++) {
            a[i * N + j] = rand() / 1000000;
        }
    }
    t = gettime() - t;

    printf("\n");
    printf("\tMemory: %f sec\n", t);
    printf("\n");
}

void stats(void) {
    double avg[2] = {0, 0};
    double min[2] = {DBL_MAX, DBL_MAX};
    double max[2] = {0, 0};
    double x;

    printf("\tSize: %d\n", N);
    printf("\tRuns: %d\n", RUNS);
    printf("\tSubruns: %d\n", SUBRUNS);
    printf("\n");

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
