#define sz           long long

#define M(n)         calloc(n,sizeof(sz))
#define C(d,s,n)     memcpy(d,s,(n)*sizeof(sz))
#define R(x,n)       realloc(x,(n)*sizeof(sz))

#define DO(n,x)      {sz $n=(n),i=0;for(;i<$n;++i){x;}}
#define DOj(n,x)     {sz $n=(n),j=0;for(;j<$n;++j){x;}}
#define DOk(n,x)     {sz $n=(n),k=0;for(;k<$n;++k){x;}}
#define DO2(n,m,x)   {DO((n),DOj((m),x))}
#define DO3(n,m,l,x) {DO(n,DOj(m,DOk(l,x)))}

#define P            printf
#define PA(a)        DO(L(a),P("%lli ",a[i]));P("\n") // Print array
#define PP(n,a)      DO(n,P("%lli ",a[i]));P("\n")    // Print array as pointer

#define _(e...)      ({e;})                           // Turn e into an r-value

#define  $(p,x)      if(p){x;}
#define $$(p,x)      else if(p){x;}
#define E(x...)      else{x;}

#define  S(x,a...) switch(x){a}
#define Cs(x,a...) case x:{a;}break;

#define CHECK(e, v)  $((e)==v,printf("%lli\n",(e))) else printf("Assertion %s == %s failed\n", #e, #v);
