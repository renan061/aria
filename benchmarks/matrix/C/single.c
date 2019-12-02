#include <stdio.h>
#include <stdlib.h>

#include "common.c"

void mul(void) {
    for (int i = 0; i < N; i++) {
        r[i] = 0;
        for (int j = 0; j < N; j++) {
            r[i] += a[i][j] * b[j];
        }
    }
}

int main(void) {
    init();
    for (int i = 0; i < RUNS; i++) {
        START;
        mul();
        END;
    }
    stats();
    return 0;
}
