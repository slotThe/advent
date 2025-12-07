#include "util.h"

int main() {
    FILE *f=fopen("../inputs/day07.txt", "r");
    I c,s,i=0;W((c=fgetc(f))!='\n',$(c=='S',s=i);++i);
    i=0; U o=0,bs[142];DO(142,bs[i]=0);bs[s]=1; // init beams
    W((c=fgetc(f))!=EOF,
      $(c=='\n',i=0;continue;);                 // reset for next line
      $(c=='^' && bs[i]!=0, o++; bs[i+1]+=bs[i]; bs[i-1]+=bs[i]; bs[i]=0;); // flow down
      i++);                                     // advance character
    CHECKP("%lld\n", o, 1681);
    U t=0;DO(142,t+=bs[i]);CHECKP("%lld\n", t, 422102272495018);
    fclose(f);
}
