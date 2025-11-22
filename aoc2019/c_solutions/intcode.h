#pragma once

#include "util.h"

#define ADD                  1
#define MUL                  2
#define INP                  3
#define OUT                  4
#define JIT                  5 // Jump if true
#define JIF                  6 // Jump if false
#define LT                   7
#define EQ                   8
#define RB                   9 // Relative base
#define HALT_AND_CATCH_FIRE 99

typedef enum { succ, need_inp } EC; // Exit code

typedef struct { // Intcode
  sz *t; // tape; capacity 4096; length at -1, cur pos at -2, relative base at -3
  sz *i; // inp ; capacity  128; length at -1, cur pos at -2
  sz *o; // outp; capacity  128; length at -1, always at the end
} IC;

#define RE(e)  free(op); return e; // Clean up + return from execution with exit code

#define it     ic->t    // intcode token
#define ii     ic->i    // intcode input
#define io     ic->o    // intcode output

#define il     ii[-1]   // input  length
#define ol     io[-1]   // output length
#define tl     it[-1]   // token  length
#define ip     ii[-2]   // input  position
#define tp     it[-2]   // token  position
#define rb     it[-3]   // relative base

#define lout   io[ol-1] // Get last output

// 0=position-mode 1=immediate-mode 2=param-mode
#define ixr(x) _(sz $x=(x),j=tp+$x;op[$x]==1?it[j]:it[it[j]+(op[$x]==2?rb:0)]) // read
#define ixw(x) _(sz $x=(x),j=tp+$x;it[j]+(op[$x]==2?rb:0))                     // write

IC*  ic_new          (sz *tape, sz n, sz *in, sz m);
void ic_push_inp     (IC *ic, sz *xs, sz n);
void ic_free         (IC *ic);
void ic_kill         (IC *ic);
EC   ic_execute      (IC *ic);
sz   ic_run_util_fire(sz *tape, sz n, sz in);
