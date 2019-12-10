#include <assert.h>
#include <stdio.h>
#include <time.h>

// configuration
const int subruns = 100;

const double x0 = 0;
const double xN = 10;
const double rectangles = 100000;

// -----------------------------------------------------------------------------

const double section = (xN - x0) / rectangles;

double f(double x);
double gettime(void);

double calc_single(void) {
    double area = 0.0;
    double halfsection = section / 2.0;
    for (double x = x0; x < rectangles; x++) {
        area += f(x * section + halfsection) * section;
    }
    return area;
}

int main(void) {
    printf("section: %f\n", section);

    double area;

    double t = gettime();
    for (int i = 0; i < subruns; i++) {
        area = calc_single();
    }
    t = gettime() - t;

    printf("area: %lf\n", area);
    printf("t: %f\n", t);

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
