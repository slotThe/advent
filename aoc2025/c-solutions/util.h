#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef int I; typedef char C;
#define $(x,e)  if(x){e;}
#define E(e)    else{e;}
#define P       printf
#define DO(n,e) {I $n=(n),i=0;for(;i<$n;++i){e;}}
#define W(x,e)  while(x){e;}
#define DW(e,x) do{e;}while(x)
#define CHECK(e, v) $((e)==v,printf("%i\n",(e)))E(printf("Assertion %s == %s failed\n", #e, #v))

#define sgn(x) ((x)==0?0:(x)>0?1:-1)
#define abs(x) (sgn(x)*(x))
