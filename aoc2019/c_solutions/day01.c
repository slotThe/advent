#include "util.h"

int main() {
    sz n=0,o=0,t=0,*ns=read_ints("../inputs/day01.txt", &n);
    DO(n, _(o+=ns[i]/3-2;
            int tmp=0,el=ns[i];while(el>0){el=el/3-2;tmp+=el;}t+=tmp;));
    CHECK(o, 3334282);
    CHECK(t, 4998467);
    free(ns);
}
