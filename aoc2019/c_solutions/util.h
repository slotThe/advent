#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// :]

#define M(n)        calloc(n,sizeof(int))
#define C(d,s,n)    memcpy(d,s,(n)*sizeof(int))
#define R(x,n)      realloc(x,(n)*sizeof(int))
#define L(x)        sizeof(x)/sizeof(x[0])
#define DO(n,x)     {int $n=(n),i=0;for(;i<$n;++i){x;}}
#define DOj(n,x)    {int $n=(n),j=0;for(;j<$n;++j){x;}}
#define DOk(n,x)    {int $n=(n),k=0;for(;k<$n;++k){x;}}
#define DO2(n,m,x)  {DO((n),DOj((m),x))}
#define DO3(n,m,l,x){DO(n,DOj(m,DOk(l,x)))}
#define P           printf
#define PA(a)       DO(L(a),P("%i ",a[i]));P("\n") // Print array
#define PP(a,n)     DO(n,P("%i ",a[i]));P("\n")    // Print array as pointer
#define _(e...)     ({e;})         // Turn e into an r-value
#define  $(p,x)     if(p){x;}      // if-then
#define $$(p,x)     else if(p){x;}
#define CHECK(PRNT, EXPR, VAL)                                                 \
    if (EXPR == VAL)                                                           \
        printf(PRNT, EXPR);                                                    \
    else                                                                       \
        printf("Assertion %s == %s failed\n", #EXPR, #VAL);

static inline char* readf(char const *const pth, int *l) {
    FILE *file = fopen(pth, "r");
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    char *buffer = malloc(len + 1);
    $(buffer, fseek(file, 0, SEEK_SET); fread(buffer, 1, len, file); buffer[len] = '\0');
    fclose(file);
    *l=len;
    return buffer;
}

static inline char **read_lines(char const *const pth, int *out_n) {
    FILE *file=fopen(pth, "r"); int ch,l=0;
    do { ch=fgetc(file); $(ch=='\n',l++) } while (ch != EOF); rewind(file);
    char **ls=calloc(l,sizeof(char*)); size_t n; int r;
    DO(l, n=0; r=getline(&ls[i],&n,file); ls[i][r-1]='\0');
    fclose(file);
    *out_n=l;
    return ls;
}

static inline int *read_ints(char const *const pth, int *out_n) {
    int max=16, *ns=M(max), n=0, _=0;
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

static inline void rev(int *xs, int n) {
  int *l=xs,*r=&xs[n-1];
  while(l<r){int tmp=*l;*l++=*r;*r--=tmp;}
}
