#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define N 1000

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

void mul(float matrix[N][N], float array[N], float r[N]) {
    for (int i = 0; i < N; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += matrix[i][j] * array[j];
        }
    }
}

int main(void) {
    float a[N][N], b[N], c[N];

    srand(time(NULL));
    for (int i = 0; i < N; i++) {
        b[i] = (rand() % 1000) / 10.0;
        for (int j = 0; j < N; j++) {
            a[i][j] = (rand() % 1000) / 10.0;
        }
    }

    mul(a, b, c);

    return 0;
}
