#include <assert.h>
#include <float.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>

// configuration
#define runs     10
#define subruns  10
#define threads  8

const double x0 = 0;
const double xN = 16;
const double rectangles = 80000000;

#define result 5706

// -----------------------------------------------------------------------------

const int single = 0;
const int multi  = 1;

const int thread_rectangles = rectangles / threads;
const double section = (xN - x0) / rectangles;
const double halfsection = section / 2.0;

double partials[2][runs];

double f(double x);
double gettime(void);
void run(int type, double (*f)(void));
void stats(void);

// -----------------------------------------------------------------------------

double calc_single(void) {
    double area = 0.0;
    for (double x = x0; x < rectangles; x++) {
        area += f(x * section + halfsection) * section;
    }
    return area;
}

// -----------------------------------------------------------------------------

typedef struct Range {
    int start;
    double area;
} Range;

void* calc_range(void* param) {
    Range* range = (Range*)param;
    double area = 0.0;
    int start = range->start * thread_rectangles;
    int end = start + thread_rectangles;
    for (double x = start; x < end; x++) {
        area += f(x * section + halfsection) * section;
    }
    range->area = area;
    return NULL;
}

double calc_multi(void) {
    pthread_t ids[threads];
    Range ranges[threads];
    for (int i = 0; i < threads; i++) {
        ranges[i].start = i;
        pthread_create(&ids[i], NULL, calc_range, &ranges[i]);
    }
    double area = 0;
    for (int i = 0; i < threads; i++) {
        pthread_join(ids[i], NULL);
        area += ranges[i].area;
    }
    return area;
}

// -----------------------------------------------------------------------------

int main(void) {
    run(single, calc_single);
    run(multi, calc_multi);
    stats();
    return 0;
}

// -----------------------------------------------------------------------------

double f(double x) {
    return 5 * x * x - 10 * x + 10;
}

double gettime(void) {
    struct timespec ts;
    assert(!clock_gettime(CLOCK_MONOTONIC, &ts)); // CLOCK_REALTIME
    return ((double)(ts.tv_sec) + ((double)(ts.tv_nsec) / 1e9));
}

void run(int type, double (*f)(void)) {
    double t, area;
    for (int i = 0; i < runs; i++) {
        t = gettime();
        for (int j = 0; j < subruns; j++) {
            area = f();
            assert((int)area == result);
        }
        partials[type][i] = gettime() - t;
    }
}

void stats(void) {
    double avg[2] = {0, 0};
    double min[2] = {DBL_MAX, DBL_MAX};
    double max[2] = {0, 0};
    double x;

    printf("\tRectangles: %d\n", (int)rectangles);
    printf("\tRuns: %d\n", runs);
    printf("\tSubruns: %d\n", subruns);
    printf("\n");

    for (int i = 0; i < 2; i++) {
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
        if (i == single) {
            printf("SINGLE");
        } else {
            printf("MULTI (%d threads)", threads);
        }
        printf("\n");
        printf("\tAvg: %f sec\n", avg[i]);
        printf("\tMax: %f sec (+%f)\n", max[i], diffmax);
        printf("\tMin: %f sec (-%f)\n", min[i], diffmin);
        printf("\t- %f%%\n", diffmin / avg[i] * 100);
        printf("\t+ %f%%\n", diffmax / avg[i] * 100);
        printf("\n");
    }

    double speedup = avg[single] / avg[multi];
    printf("\tSpeedup: %.2f\n", speedup);
    printf("\tThread Efficiency: %.2f%%\n", speedup / threads * 100);
    printf("\n");
}
