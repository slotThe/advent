#include <stdlib.h>
#include "util.h"
#include <stdbool.h>

bool two_adj2(int xs[6]) { // ∨´·0‿1‿0⊸⍷«⊸=
    bool n[7]; n[0]=0; n[6]=0; // neighbouring
    DO(6, n[i+1] = xs[i] == xs[i+1]);
    DO(6, if (!n[i] && n[i+1] && !n[i+2]) return 1);
    return 0;
}

int main() {
    int _; char *buffer = readf("../inputs/day04.txt",&_); char *p = buffer;
    int beg = strtol(p, &p, 10); ++p; int end = strtol(p, &p, 10);
    free(buffer);
    int one=0, two=0;
    for (int n = beg; n < end; ++n) {
        int xs[6], k=n; DO(6, xs[5-i] = k % 10; k /= 10);
        bool nd =    _(bool x=1; DO(5, x = x && xs[i] <= xs[i+1]); x);
        one += nd && _(bool x=0; DO(5, x = x || xs[i] == xs[i+1]); x);
        two += nd && two_adj2(xs);
    }
    CHECK("%i\n", one, 979);
    CHECK("%i\n", two, 635);
}
