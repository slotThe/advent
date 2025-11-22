#include "util.h"

int main() {
  sz n=0; char **inp = read_lines("../inputs/day06.txt", &n);
  char *l[n], *r[n];
  DO(n, l[i]=calloc(4,sizeof(char));DOj(3,l[i][j]=inp[i][  j]);l[i][3]='\0';
        r[i]=calloc(4,sizeof(char));DOj(3,r[i][j]=inp[i][4+j]);l[i][3]='\0');

  // ix[i]=j means l[i] is connected with r[j], so starting with some s and
  // repeatedly doing s=ix[s] lets us walk a path backwards.
  int ix[n];DO(n,ix[i]=n);DO2(n,n,if(!strcmp(l[i],r[j]))ix[i]=j);

  #define W(x,d) p=ix[x];(d);while(p<n){p=ix[p];(d);} // Walk
  sz c=0,p=0; DO(n, W(i,++c));
  CHECK(c, 186597);

  sz y=0,s=0; DO(n,$(!strcmp("YOU",r[i]),y=i)); DO(n,$(!strcmp("SAN",r[i]),s=i));
  sz yc[n],yi=0;W(y,yc[yi++]=p);rev(yc,yi);
  sz sc[n],si=0;W(s,sc[si++]=p);rev(sc,si);
  sz *yp=yc,*sp=sc,i=0;while(*yp++==*sp++){++i;}
  CHECK(yi-i+si-i, 412);

  DO(n,free(l[i]);free(r[i]);free(inp[i]));free(inp);
}
