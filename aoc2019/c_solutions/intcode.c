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

#define TCP sz *op, IC *ic /* tail call params */
typedef EC (*TCF)(TCP);    /* tail call function */
static TCF ops[];          /* function array */
#define TC(n) free(op); op=opcode(it[tp+=(n)]); [[clang::musttail]] return ops[op[0]](op,ic); /* tail call */
EC op_add(TCP) { it[zw]=x+y; TC(4) }                                // 1,x,y,z
EC op_mul(TCP) { it[zw]=x*y; TC(4) }                                // 2,x,y,z
EC op_inp(TCP) { it[xw]=_($(ip>=il,RE(need_inp));ii[ip++]); TC(2) } // 3,x
EC op_out(TCP) { io[ol++]=x; TC(2) }                                // 4,x
EC op_jit(TCP) { TC(x?-tp+y:3    ) }                                // 5,x,y
EC op_jif(TCP) { TC(x?3    :-tp+y) }                                // 6,x,y
EC op_lt (TCP) { it[zw]=x<y; TC(4) }                                // 7,x,y,z
EC op_eq (TCP) { it[zw]=x==y; TC(4) }                               // 8,x,y,z
EC op_rb (TCP) { rb+=x; TC(2) }                                     // 9,x
EC op_die(TCP) { RE(succ) }                                         // 99
EC ic_execute(IC *ic) { sz *op=opcode(it[tp]); return (ops[op[0]])(op,ic); }
static TCF ops[128]={ [ADD]=&op_add, [MUL]=&op_mul, [INP]=&op_inp, [OUT]=&op_out, [JIT]=&op_jit, [JIF]=&op_jif, [LT]=&op_lt, [EQ]=&op_eq, [RB]=&op_rb, [HALT_AND_CATCH_FIRE]=&op_die, };

sz ic_run_util_fire(sz *tape, sz n, sz in) {
    sz a[1]={in};
    IC *ic=ic_new(tape, n, a, 1); ic_execute(ic); sz o=lout; ic_kill(ic); return o;
}
