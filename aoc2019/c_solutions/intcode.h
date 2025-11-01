#pragma once

#include <stdlib.h>

#define ADD 1
#define MUL 2
#define HALT_AND_CATCH_FIRE 99

typedef struct {
    int *ins;    // length at -1
} intcode;

intcode *ic_new(int const *const ins, int const n);

void ic_kill(intcode *ic);

void ic_set(intcode *ic, int n, ...);

int ic_get(intcode *ic, size_t const i);

int ic_ixix(intcode const *const ic, int const n);

void ic_execute(intcode *ic);
