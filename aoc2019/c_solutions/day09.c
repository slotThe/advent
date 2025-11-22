#include "intcode.h"

int main() {
  sz n,*inp=read_ints("../inputs/day09.txt",&n);
  CHECK(ic_run_util_fire(inp,n,1),2457252183);
  CHECK(ic_run_util_fire(inp,n,2),70634);
  free(inp);
}
