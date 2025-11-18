#include "util.h"
#include "intcode.h"

int main() {
  int n, *inp = read_ints("../inputs/day05.txt", &n);

  int in[1]={1}; IC *ic=ic_new(inp, n, in, 1); ic_execute(ic);
  CHECK("%i\n", lout, 9654885);

  ic_kill(ic); in[0]=5; ic=ic_new(inp, n, in, 1); ic_execute(ic);
  CHECK("%i\n", lout, 7079459);

  ic_kill(ic); free(inp);
}
