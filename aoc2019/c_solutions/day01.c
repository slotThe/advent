#include "util.h"
#include <stdlib.h>

int main() {
    int n=0, *ns=read_ints("../inputs/day01.txt", &n);

    int one=0,two=0;
    DO(n, ({ one+=ns[i]/3-2;
             int tmp=0,el=ns[i];while(el>0){el=el/3-2;tmp+=el;}two+=tmp;}))
    CHECK("%i\n", one, 3334282);
    CHECK("%i\n", two, 4998467);

    free(ns);
}
