#include "intcode.h"

int main() {
    sz n=0, *ns = read_ints("../inputs/day02.txt", &n);

    IC *ic = ic_new(ns, n, NULL, 0); it[1]=12;it[2]=2; ic_execute(ic);
    CHECK(it[0],4570637); ic_kill(ic);

    DO2(100, 100, {
        IC *ic = ic_new(ns, n, NULL, 0); it[1]=i;it[2]=j; ic_execute(ic);
        $(it[0] == 19690720, CHECK((sz)100*i+j,5485); ic_kill(ic); goto haha);
        ic_kill(ic);
      });
haha:
    free(ns);
    return 0;
}
