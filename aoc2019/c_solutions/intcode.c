#include "intcode.h"

IC *ic_new(sz *tape, sz n, sz *inp, sz m) {
    IC *ic = malloc(sizeof(*ic));
    it=M(4096); *it++=0; *it++=0; *it++=n; C(it,tape,n);
    ii=M(128) ;          *ii++=0; *ii++=m; C(ii,inp ,m);
    io=M(128) ;                   *io++=0;
    return ic;
}

void ic_push_inp(IC *ic, sz *xs, sz n) {
    sz til=il; ii-=2; ii=R(ii,til+n+2); ii+=2; il+=n; DO(n,ii[i+til]=xs[i]);
}

void ic_free(IC *ic) { free(it-=3); free(ii-=2); free(io-=1); }

void ic_kill(IC *ic) { ic_free(ic); free(ic); }

sz *opcode(sz n) { // 1234 -> 34,2,1,0,0,0,0,0,0
    sz *r=M(9),op,k=n; op=k%10;k/=10;op+=k%10*10;k/=10;r[0]=op; DO(8,r[i+1]=k%10;k/=10); return r;
}

#define x      ixr(1)
#define xw     ixw(1)
#define y      ixr(2)
#define yw     ixw(2)
#define z      ixr(3)
#define zw     ixw(3)

EC ic_execute(IC *ic) {
    while (tp < tl) {
        sz *op = opcode(it[tp]);
        switch (op[0]) {
        case ADD: it[zw]=x+y                               ; tp+=4; break; // 1,x,y,z
        case MUL: it[zw]=x*y                               ; tp+=4; break; // 2,x,y,z
        case INP: it[xw]=_($(ip>=il,RE(need_inp));ii[ip++]); tp+=2; break; // 3,x
        case OUT: io[ol++]=x;                              ; tp+=2; break; // 4,x
        case JIT: $( x, tp=y)E(tp+=3)                             ; break; // 5,x,y
        case JIF: $(!x, tp=y)E(tp+=3)                             ; break; // 6,x,y
        case LT : it[zw]=x<y                               ; tp+=4; break; // 7,x,y,z
        case EQ : it[zw]=x==y                              ; tp+=4; break; // 8,x,y,z
        case RB : rb+=x                                    ; tp+=2; break; // 9,x
        case HALT_AND_CATCH_FIRE: RE(succ);                                // 99
        }
        free(op);
    }
}

sz ic_run_util_fire(sz *tape, sz n, sz in) {
    sz a[1]={in};
    IC *ic=ic_new(tape, n, a, 1); ic_execute(ic); sz o=lout; ic_kill(ic); return o;
}
