#include "intcode.h"
#include "util.h"
#include <stdarg.h>
#include <stdlib.h>

#define ix(n)    _(int $n=(n),j=tp+$n; op[$n]==pm_imm ? it[j] : it[it[j]])
#define x        ix(1)
#define xv       it[tp+1]
#define y        ix(2)
#define yv       it[tp+2]
#define z        ix(3)
#define zv       it[tp+3]

IC *ic_new(int *tape, int n, int *inp, int m) {
    IC *ic = malloc(sizeof(*ic));
    it=M(1024); *it++=0; *it++=n; C(it,tape,n);
    ii=M(128) ; *ii++=0; *ii++=m; C(ii,inp ,m);
    io=M(128) ; *io++=0;
    return ic;
}

void ic_push_inp(IC *ic, int *xs, int n) {
    int til=il; ii-=2; ii=R(ii,til+n+2); ii+=2; il+=n;
    DO(n,ii[i+til]=xs[i]);
}

void ic_free(IC *ic) { free(it-=2); free(ii-=2); free(io-=1); }

void ic_kill(IC *ic) { ic_free(ic); free(ic); }

void ic_set(IC *ic, int n, ...) {
    va_list ap; va_start(ap, n);
    for(int i=0; i<n; i+=2){ int j=va_arg(ap,int), n=va_arg(ap,int); it[j]=n; }
    va_end(ap);
}

int *opcode(int n) { // 1234 -> 34,2,1,0,0,0,0,0,0
    int *r = M(9);
    int op,k=n; op=k%10;k/=10;op+=k%10*10;k/=10;r[0]=op; DO(8,r[i+1]=k%10;k/=10);
    return r;
}

EC ic_execute(IC *ic) {
    while (tp < tl) {
        int *op = opcode(it[tp]);
        switch (op[0]) {
        case ADD: ic_set(ic,2, zv,x+y)   ; tp+=4; break; // 1,x,y,z
        case MUL: ic_set(ic,2, zv,x*y)   ; tp+=4; break; // 2,x,y,z
        case INP: ic_set(ic,2, xv,cinp)  ; tp+=2; break; // 3,x
        case OUT: sout                   ; tp+=2; break; // 4,x
        case JIT: $( x, tp=y) else tp+=3        ; break; // 5,x,y
        case JIF: $(!x, tp=y) else tp+=3        ; break; // 6,x,y
        case LT : ic_set(ic,2, zv,x<y)   ; tp+=4; break; // 7,x,y,z
        case EQ : ic_set(ic,2, zv,x==y)  ; tp+=4; break; // 8,x,y,z
        case HALT_AND_CATCH_FIRE: RE(succ);              // 99
        }
        free(op);
    }
}
