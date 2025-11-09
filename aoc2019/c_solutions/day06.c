#include "util.h"

int main() {
  int n=0;
  char **inp = read_lines("../inputs/day06.txt", &n);
  char *l[n], *r[n];
  DO(n, l[i]=M(4,char);DOJ(3,l[i][j]=inp[i][  j]);l[i][3]='\0';
        r[i]=M(4,char);DOJ(3,r[i][j]=inp[i][4+j]);l[i][3]='\0');

  // ix[i]=j means l[i] is connected with r[j], so starting with some s and
  // repeatedly doing s=ix[s] lets us walk a path backwards.
  int ix[n];DO(n,ix[i]=n);DO2(n,n,if(!strcmp(l[i],r[j]))ix[i]=j);

  #define W(st,d) j=ix[st];(d);while(j<n){j=ix[j];(d);} // Walk
  int c=0,j=0; DO(n, W(i,++c));
  CHECK("%i\n", c, 186597);

  int y=0,s=0; DO(n,if(!strcmp("YOU",r[i]))y=i); DO(n,if(!strcmp("SAN",r[i]))s=i);
  int yc[n],yi=0;W(y,yc[yi++]=j);rev(yc,yi);
  int sc[n],si=0;W(s,sc[si++]=j);rev(sc,si);
  int *yp=yc,*sp=sc,i=0;while(*yp++==*sp++){++i;}
  CHECK("%i\n", yi-i+si-i, 412);

  DO(n,free(l[i]);free(r[i]);free(inp[i]));free(inp);
}
