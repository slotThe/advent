#include "util.h"

#define M(x) (((x)%100)+100)%100
#define xi x[i]

int main() {
    FILE *file=fopen("../inputs/day01.txt", "r");
    I x[5000],i=0,j=0; C c,c2;
    W((c=fgetc(file))!=EOF,
      c2=fgetc(file); C r[4]; j=0; DW(r[j++]=c2;c2=fgetc(file), c2!='\n'); r[j]='\0';
      xi=atoi(r);$(c=='L',xi=-xi);i++);
    fclose(file);

    I z=0,d=50;DO(i,d=M(d+xi);$(d==0,++z));
    CHECK(z,1011);

    z=0,d=50;DO(i,z+=(M(d*sgn(xi))+abs(xi))/100; d=M(d+xi));
    CHECK(z,5937);
}
