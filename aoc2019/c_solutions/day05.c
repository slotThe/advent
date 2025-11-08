#include "util.h"
#include "intcode.h"

int main() {
  int n, *inp = read_ints("../inputs/day05.txt", &n);

  intcode *ic = ic_new(inp, n, 1);
  ic_execute(ic);
  CHECK("%i\n", out(ic), 9654885);

  ic_kill(ic); ic = ic_new(inp, n, 5);
  ic_execute(ic);
  CHECK("%i\n", out(ic), 7079459);

  ic_kill(ic); free(inp);
}
