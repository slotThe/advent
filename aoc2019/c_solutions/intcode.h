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

typedef enum { pm_pos, pm_imm } PM; // Param mode

typedef enum { succ, need_inp } EC; // Exit code

typedef struct { // Intcode
  int *t; // tape; capacity 1024; length at -1, cur pos at -2
  int *i; // inp ; capacity  128; length at -1, cur pos at -2
  int *o; // outp; capacity  128; length at -1, always at the end
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

#define cinp _($(ip>=il,RE(need_inp));ii[ip++]) // Consume input or bail
#define sout io[ol++]=x;                        // Set output
#define lout io[ol-1]                           // Get last output

IC*  ic_new      (int *tape, int n, int *in, int m);
void ic_push_inp (IC *ic, int *xs, int n);
void ic_free     (IC *ic);
void ic_kill     (IC *ic);
void ic_set      (IC *ic, int n, ...);
EC   ic_execute  (IC *ic);
