#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 50000

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

void mul(void) {
    for (int i = 0; i < N; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += a[i][j] * b[j];
        }
    }
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

    mul();

    return 0;
}
