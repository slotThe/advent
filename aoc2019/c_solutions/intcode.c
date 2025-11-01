#include "intcode.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

intcode *ic_new(int const *const ins, int const n) {
    intcode *ic = malloc(sizeof(*ic));
    ic->ins = calloc(n+1, sizeof(int));
    *ic->ins++ = n;
    memcpy(ic->ins, ins, n * sizeof(int));
    return ic;
}

void ic_set(intcode *ic, int n, ...) {
  va_list ap; va_start(ap, n);
  for(int i=0; i<n; i+=2) {
    int j=va_arg(ap,int);
    int n=va_arg(ap,int);
    ic->ins[j] = n;
  }
  va_end(ap);
}

void ic_kill(intcode *ic) { free(--ic->ins); free(ic); }

int ic_get(intcode *ic, size_t const i) { return ic->ins[i]; }

int ixix(intcode const *const ic, int const n) { return ic->ins[ic->ins[n]]; }

void ic_execute(intcode *ic) {
    int i=0, n=ic->ins[-1], *is=ic->ins;
    while (i < n) {
        switch (is[i]) {
        case HALT_AND_CATCH_FIRE:
            return;
        case ADD:
            ic_set(ic,2, is[i+3],ixix(ic,i+1)+ixix(ic,i+2));
            i += 4;
            break;
        case MUL:
            ic_set(ic,2, is[i+3],ixix(ic,i+1)*ixix(ic,i+2));
            i += 4;
            break;
        }
    }
}
