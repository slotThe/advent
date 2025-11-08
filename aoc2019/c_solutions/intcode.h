#pragma once

#include <stdlib.h>

#define ADD                  1
#define MUL                  2
#define INP                  3
#define OUT                  4
#define JIT                  5
#define JIF                  6
#define LT                   7
#define EQ                   8
#define HALT_AND_CATCH_FIRE 99

typedef enum { pm_pos, pm_imm } param_mode;

typedef struct {int *ins;} intcode; // length at -1, inp at -2, out at -3

intcode *ic_new(int const *const ins, int const n, int const inp);

void ic_kill(intcode *ic);

void ic_set(intcode *ic, int n, ...);

int ic_get(intcode *ic, size_t const i);

int ic_ixix(intcode const *const ic, int const n);

void ic_execute(intcode *ic);

#define len(ic)  ic->ins[-1]
#define inp(ic)  ic->ins[-2]
#define out(ic)  ic->ins[-3]

#define ix(n)    _(int $n=(n),j=i+$n; op[$n]==pm_imm ? is[j] : is[is[j]])
#define x        ix(1)
#define xv       is[i+1]
#define y        ix(2)
#define yv       is[i+2]
#define z        ix(3)
#define zv       is[i+3]
