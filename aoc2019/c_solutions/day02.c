#include "intcode.h"
#include "util.h"
#include <stdlib.h>

int main() {
    int n=0, *ns = read_ints("../inputs/day02.txt", &n);

    intcode *ic = ic_new(ns, n, 0);
    ic_set(ic,4, 1,12, 2,2);
    ic_execute(ic);
    CHECK("%i\n", ic->ins[0], 4570637);
    ic_kill(ic);

    for (int noun = 0; noun < 100; ++noun) {
        for (int verb = 0; verb < 100; ++verb) {
            intcode *ic = ic_new(ns, n, 0);
            ic_set(ic,4, 1,noun, 2,verb);
            ic_execute(ic);
            if (ic->ins[0] == 19690720) {
                CHECK("%i\n", 100 * noun + verb, 5485);
                ic_kill(ic);
                goto haha;
            }
            ic_kill(ic);
        }
    }
haha:
    free(ns);
    return 0;
}
