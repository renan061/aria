#include <assert.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <time.h>

// configuration
#define runs        10
#define subruns     100
#define size        10000

#define nthreads 4
const int arraythreads[nthreads] = {1, 2, 4, 8};

// -----------------------------------------------------------------------------

int threads;
int section;

double partials[nthreads + 1][runs];

double *a, *b, *c;

double gettime(void);
void memory(void);
void stats(void);
void printarray(float array[size]);

// -----------------------------------------------------------------------------

void* mul_multi(void* param) {
    int thread = *(int*)(param);
    int start = thread * section;
    int end = start + section;
    int ioff;
    double x;

    // printf("size: %d, threads: %d\n", size, threads);
    // printf("start: %d, end: %d\n", start, end);

    for (int i = start; i < end; i++) {
        x = 0;
        ioff = i * size;
        for (int j = 0; j < size; j++) {
            x += a[ioff + j] * b[j];
        }
        c[i] = x;
    }

    return NULL;
}

void mul_single(void) {
    int ioff;
    double x;
    for (int i = 0; i < size; i++) {
        x = 0;
        ioff = i * size;
        for (int j = 0; j < size; j++) {
            x += a[ioff + j] * b[j];
        }
        c[i] = x;
    }
}

// -----------------------------------------------------------------------------

int main(void) {
    memory();
    double t;

    for (int i = 0; i < runs; i++) {
        t = gettime();
        for (int j = 0; j < subruns; j++) {
            mul_single();
        }
        partials[0][i] = gettime() - t;
    }

    for (int k = 0; k < nthreads; k++) {
        threads = arraythreads[k];
        section = size / threads;
        for (int i = 0; i < runs; i++) {
            t = gettime();
            for (int j = 0; j < subruns; j++) {
                pthread_t pthreads[threads];
                int ids[threads];
                for (int x = 0; x < threads; x++) {
                    ids[x] = x;
                    pthread_create(&pthreads[x], NULL, mul_multi, &ids[x]);
                }
                for (int x = 0; x < threads; x++) {
                    pthread_join(pthreads[x], NULL);
                }
            }
            partials[k + 1][i] = gettime() - t;
        }
    }

    // TODO : assert?

    stats();
    return 0;
}

// -----------------------------------------------------------------------------

double gettime(void) {
    struct timespec ts;
    assert(!clock_gettime(CLOCK_MONOTONIC, &ts)); // CLOCK_REALTIME
    return ((double)(ts.tv_sec) + ((double)(ts.tv_nsec) / 1e9));
}

void memory(void) {
    double t = gettime();
    srand(time(0));
    a = (double*)malloc(size * size * sizeof(double));
    b = (double*)malloc(size * sizeof(double));
    c = (double*)malloc(size * sizeof(double));
    for (int i = 0; i < size; i++) {
        b[i] = rand() / 1000000;
        for (int j = 0; j < size; j++) {
            a[i * size + j] = rand() / 1000000;
        }
    }
    t = gettime() - t;

    printf("\n");
    printf("\tMemory: %f sec\n", t);
    printf("\n");
}

void stats(void) {
    int sz = nthreads + 1;
    double avg[sz];
    double min[sz];
    double max[sz];
    for (int i = 0; i < sz; i++) {
        avg[i] = max[i] = 0;
        min[i] = DBL_MAX;
    }
    double x;

    printf("\tMatrix Size: %d\n", size);
    printf("\tRuns: %d\n", runs);
    printf("\tSubruns: %d\n", subruns);
    printf("\n");

    for (int i = 0; i < sz; i++) {
        for (int j = 0; j < runs; j++) {
            x = partials[i][j];
            avg[i] += x;
            min[i] = (x < min[i]) ? x : min[i];
            max[i] = (x > max[i]) ? x : max[i];
        }

        avg[i] /= runs;

        double diffmax = max[i] - avg[i];
        double diffmin = avg[i] - min[i];

        printf("\t");
        if (i == 0) {
            printf("SINGLE");
        } else {
            printf("MULTI (%d threads)", arraythreads[i - 1]);
        }
        printf("\n");
        printf("\tAvg: %f sec\n", avg[i]);
        printf("\tMax: %f sec (+%f)\n", max[i], diffmax);
        printf("\tMin: %f sec (-%f)\n", min[i], diffmin);
        printf("\t- %f%%\n", diffmin / avg[i] * 100);
        printf("\t+ %f%%\n", diffmax / avg[i] * 100);
        printf("\n");

        if (i == 0) { continue; }

        double speedup = avg[0] / avg[i];
        double efficiency = speedup / arraythreads[i - 1] * 100;
        printf("\tSpeedup: %.2f\n", speedup);
        printf("\tEfficiency: %.2f%%\n", efficiency);
        printf("\n");
    }
}
