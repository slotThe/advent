#include "intcode.h"
#include "util.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

intcode *ic_new(int const *const ins, int const n, int const inp) {
    intcode *ic = malloc(sizeof(*ic));
    ic->ins=M(n+3,int); *ic->ins++=0; *ic->ins++=inp; *ic->ins++=n;
    memcpy(ic->ins, ins, n * sizeof(int));
    return ic;
}

void ic_kill(intcode *ic) { free(ic->ins-=3); free(ic); }

void ic_set(intcode *ic, int n, ...) {
  va_list ap; va_start(ap, n);
  for(int i=0; i<n; i+=2){ int j=va_arg(ap,int), n=va_arg(ap,int); ic->ins[j]=n; }
  va_end(ap);
}

int *opcode(int n) { // 1234 -> 34,2,1,0,0,0,0,0,0
    int *r = M(9,int);
    int op,k=n; op=k%10;k/=10;op+=k%10*10;k/=10;r[0]=op; DO(8,r[i+1]=k%10;k/=10);
    return r;
}

void ic_execute(intcode *ic) {
    int i=0, n=len(ic), *is=ic->ins;
    while (i < n) {
        int *op = opcode(is[i]);
        switch (op[0]) {
        case HALT_AND_CATCH_FIRE: free(op); return;
        case ADD: ic_set(ic,2, zv,x+y)    ; i += 4; break;  // 1,x,y,z
        case MUL: ic_set(ic,2, zv,x*y)    ; i += 4; break;  // 2,x,y,z
        case INP: ic_set(ic,2, xv,inp(ic)); i += 2; break;  // 3,x
        case OUT: out(ic)=x               ; i += 2; break;  // 4,x
        case JIT: $( x, i=y) i+=3                 ; break;  // 5,x,y
        case JIF: $(!x, i=y) i+=3                 ; break;  // 6,x,y
        case LT : ic_set(ic,2, zv,x<y)    ; i += 4; break;  // 7,x,y,z
        case EQ : ic_set(ic,2, zv,x==y)   ; i += 4; break;  // 8,x,y,z
        }
        free(op);
    }
}
