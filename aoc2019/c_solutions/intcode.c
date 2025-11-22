#include "intcode.h"
#include "util.h"
#include <stdarg.h>
#include <stdlib.h>

#define ixr(n)   _(sz $n=(n),j=tp+$n; op[$n]==pm_imm ? it[j] : it[it[j]+(op[$n]==pm_rel?rb:0)]) // read
#define ixw(n)   _(sz $n=(n),j=tp+$n; it[j]+(op[$n]==pm_rel?rb:0)) // write
#define x        ixr(1)
#define xw       ixw(1)
#define y        ixr(2)
#define yw       ixw(2)
#define z        ixr(3)
#define zw       ixw(3)

IC *ic_new(sz *tape, sz n, sz *inp, sz m) {
    IC *ic = malloc(sizeof(*ic));
    it=M(4096); *it++=0; *it++=0; *it++=n; C(it,tape,n);
    ii=M(128) ;          *ii++=0; *ii++=m; C(ii,inp ,m);
    io=M(128) ;                   *io++=0;
    return ic;
}

void ic_push_inp(IC *ic, sz *xs, sz n) {
    sz til=il; ii-=2; ii=R(ii,til+n+2); ii+=2; il+=n;
    DO(n,ii[i+til]=xs[i]);
}

void ic_free(IC *ic) { free(it-=3); free(ii-=2); free(io-=1); }

void ic_kill(IC *ic) { ic_free(ic); free(ic); }

void ic_set(IC *ic, sz n, ...) {
    va_list ap; va_start(ap, n);
    for(sz i=0; i<n; i+=2){ sz j=va_arg(ap,sz), n=va_arg(ap,sz); it[j]=n; }
    va_end(ap);
}

sz *opcode(sz n) { // 1234 -> 34,2,1,0,0,0,0,0,0
    sz *r = M(9);
    sz op,k=n; op=k%10;k/=10;op+=k%10*10;k/=10;r[0]=op; DO(8,r[i+1]=k%10;k/=10);
    return r;
}

EC ic_execute(IC *ic) {
    while (tp < tl) {
        sz *op = opcode(it[tp]);
        switch (op[0]) {
        case ADD: ic_set(ic,2, zw,x+y)   ; tp+=4; break; // 1,x,y,z
        case MUL: ic_set(ic,2, zw,x*y)   ; tp+=4; break; // 2,x,y,z
        case INP: ic_set(ic,2, xw,cinp)  ; tp+=2; break; // 3,x
        case OUT: io[ol++]=x;            ; tp+=2; break; // 4,x
        case JIT: $( x, tp=y) else tp+=3        ; break; // 5,x,y
        case JIF: $(!x, tp=y) else tp+=3        ; break; // 6,x,y
        case LT : ic_set(ic,2, zw,x<y)   ; tp+=4; break; // 7,x,y,z
        case EQ : ic_set(ic,2, zw,x==y)  ; tp+=4; break; // 8,x,y,z
        case RB : rb+=x                  ; tp+=2; break; // 9,x
        case HALT_AND_CATCH_FIRE: RE(succ);              // 99
        }
        free(op);
    }
}

sz ic_run_util_fire(sz *tape, sz n, sz in) {
    sz a[1]={in};
    IC *ic=ic_new(tape, n, a, 1); ic_execute(ic); sz o=lout; ic_kill(ic); return o;
}
