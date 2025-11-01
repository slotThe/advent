#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *read_ints(char const *const pth, size_t *out_n) {
    size_t max = 16;
    int *ns = calloc(max, sizeof(int));

    FILE *file = fopen(pth, "r");
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    char *buffer = malloc(len + 1);
    if (buffer) {
        fseek(file, 0, SEEK_SET);
        fread(buffer, 1, len, file);
        buffer[len] = '\0';
    }
    fclose(file);

    size_t n = 0;
    char *p = buffer;
    while (*p) {
        if (n >= max) {
            max *= 2;
            ns = realloc(ns, max * sizeof(int));
        }
        ns[n++] = strtol(p, &p, 10);
        for (; !(*p == '-' || *p >= '0' && *p <= '9') && *p; ++p)
            ;
    }

    free(buffer);
    *out_n = n;
    return ns;
}

#define CHECK(PRNT, EXPR, VAL)                                                 \
    if (EXPR == VAL)                                                           \
        printf(PRNT, EXPR);                                                    \
    else                                                                       \
        printf("Assertion %s == %s failed\n", #EXPR, #VAL);

// :]

#define DO(n, x)                                                               \
    {                                                                          \
        int i = 0, $n = (n);                                                   \
        for (; i < $n; ++i) {                                                  \
            x;                                                                 \
        }                                                                      \
    }
