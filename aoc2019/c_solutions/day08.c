#include "util.h"

int main() {
    sz len; char* buf=readf("../inputs/day08.txt", &len); sz nl=len/150, ls[nl][150];

    DO2(nl,150,ls[i][j]=(sz)buf[i*150+j]-48);
    sz m=9999,l=0;DO(nl,sz z=0;DOj(150,$(ls[i][j]==0,++z));$(z<m,m=z;l=i));
    sz o=0,t=0;DO(150,$(ls[l][i]==1,++o)$$(ls[l][i]==2,++t););
    CHECK(o*t,1806);

    sz im[150];DO(150,im[i]=2);
    DO(150,$(im[i]==2,DOj(nl,$(ls[j][i]!=2,im[i]=ls[j][i];break))));
    DO(6,DOj(25,$(im[i*25+j],P("█"))E(P(" ")));P("\n")); // JAFRA
}
