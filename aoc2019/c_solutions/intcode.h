#pragma once

#include <stdlib.h>
#include "util.h"

#define ADD                  1
#define MUL                  2
#define INP                  3
#define OUT                  4
#define JIT                  5
#define JIF                  6
#define LT                   7
#define EQ                   8
#define RB                   9
#define HALT_AND_CATCH_FIRE 99

typedef enum { pm_pos, pm_imm, pm_rel } PM; // Param mode

typedef enum { succ, need_inp } EC; // Exit code

typedef struct { // Intcode
  sz *t; // tape; capacity 4096; length at -1, cur pos at -2, relative base at -3
  sz *i; // inp ; capacity  128; length at -1, cur pos at -2
  sz *o; // outp; capacity  128; length at -1, always at the end
} IC;

#define RE(e) free(op); return e; // Clean up + return from execution with exit code

#define it ic->t // intcode token
#define ii ic->i // intcode input
#define io ic->o // intcode output

#define il ii[-1] // input  length
#define ol io[-1] // output length
#define tl it[-1] // token  length
#define ip ii[-2] // input  position
#define tp it[-2] // token  position
#define rb it[-3] // relative base

#define cinp _($(ip>=il,RE(need_inp));ii[ip++]) // Consume input or bail
#define lout io[ol-1]                           // Get last output

IC*  ic_new          (sz *tape, sz n, sz *in, sz m);
void ic_push_inp     (IC *ic, sz *xs, sz n);
void ic_free         (IC *ic);
void ic_kill         (IC *ic);
void ic_set          (IC *ic, sz n, ...);
EC   ic_execute      (IC *ic);
sz   ic_run_util_fire(sz *tape, sz n, sz in);
