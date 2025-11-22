#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define sz           long long
#define M(n)         calloc(n,sizeof(sz))
#define C(d,s,n)     memcpy(d,s,(n)*sizeof(sz))
#define R(x,n)       realloc(x,(n)*sizeof(sz))
#define L(x)         sizeof(x)/sizeof(x[0])
#define DO(n,x)      {sz $n=(n),i=0;for(;i<$n;++i){x;}}
#define DOj(n,x)     {sz $n=(n),j=0;for(;j<$n;++j){x;}}
#define DOk(n,x)     {sz $n=(n),k=0;for(;k<$n;++k){x;}}
#define DO2(n,m,x)   {DO((n),DOj((m),x))}
#define DO3(n,m,l,x) {DO(n,DOj(m,DOk(l,x)))}
#define P            printf
#define PA(a)        DO(L(a),P("%lli ",a[i]));P("\n") // Print array
#define PP(n,a)      DO(n,P("%lli ",a[i]));P("\n")    // Print array as pointer
#define _(e...)      ({e;})         // Turn e into an r-value
#define  $(p,x)      if(p){x;}      // if-then
#define $$(p,x)      else if(p){x;}
#define CHECK(e, v)  $((e)==v,printf("%lli\n",(e))) else printf("Assertion %s == %s failed\n", #e, #v);

static inline char* readf(char const *const pth, sz *l) {
    FILE *file = fopen(pth, "r");
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    char *buffer = malloc(len + 1);
    $(buffer, fseek(file, 0, SEEK_SET); fread(buffer, 1, len, file); buffer[len] = '\0');
    fclose(file);
    *l=len;
    return buffer;
}

static inline char **read_lines(char const *const pth, sz *out_n) {
    FILE *file=fopen(pth, "r"); sz ch,l=0;
    do { ch=fgetc(file); $(ch=='\n',l++) } while (ch != EOF); rewind(file);
    char **ls=calloc(l,sizeof(char*)); size_t n; sz r;
    DO(l, n=0; r=getline(&ls[i],&n,file); ls[i][r-1]='\0');
    fclose(file);
    *out_n=l;
    return ls;
}

static inline sz *read_ints(char const *const pth, sz *out_n) {
    sz max=16, *ns=M(max), n=0, _=0;
    char *buffer = readf(pth,&_), *p=buffer;
    while (*p) {
        $(n>=max, max*=2; ns=R(ns,max)); // grow
        ns[n++] = strtol(p, &p, 10);
        for (; !(*p == '-' || *p >= '0' && *p <= '9') && *p; ++p);
    }
    free(buffer);
    *out_n = n;
    return ns;
}

static inline void rev(sz *xs, sz n) {
  sz *l=xs,*r=&xs[n-1];
  while(l<r){int tmp=*l;*l++=*r;*r--=tmp;}
}
