#include "intcode.h"

int main() {
  sz n, *inp = read_ints("../inputs/day05.txt", &n);
  CHECK(ic_run_util_fire(inp,n,1), 9654885);
  CHECK(ic_run_util_fire(inp,n,5), 7079459);
  free(inp);
}
