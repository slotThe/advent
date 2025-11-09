#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// :]

#define M(n,t) calloc(n,sizeof(t))
#define DOJ(n, x)  {int $n=(n),j=0;for(;j<$n;++j){x;}}
#define DO(n, x)   {int $n=(n),i=0;for(;i<$n;++i){x;}}
#define DO2(n,m,x) {DO(n,DOJ(m,x))}
#define P printf
#define R return
#define _(e...) ({e;})     // turn e into an r-value
#define $(p,n) if(p)n;else // if-then-else

static inline char* readf(char const *const pth) {
    FILE *file = fopen(pth, "r");
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    char *buffer = malloc(len + 1);
    if (buffer) {
        fseek(file, 0, SEEK_SET);
        fread(buffer, 1, len, file);
        buffer[len] = '\0';
    }
    fclose(file);
    return buffer;
}

static inline char **read_lines(char const *const pth, int *out_n) {
    FILE *file = fopen(pth, "r");
    int ch, l=0;
    do { ch=fgetc(file); if(ch=='\n')l++; } while (ch != EOF); rewind(file);
    char **ls=M(l,char*); size_t n; int r;
    DO(l, n=0; r=getline(&ls[i],&n,file); ls[i][r-1]='\0');
    fclose(file);
    *out_n=l;
    return ls;
}

static inline int *read_ints(char const *const pth, int *out_n) {
    int max = 16;
    int *ns = M(max,int);

    int n = 0;
    char *buffer = readf(pth);
    char *p = buffer;
    while (*p) {
        if (n >= max) {
            max *= 2;
            ns = realloc(ns, max * sizeof(int));
        }
        ns[n++] = strtol(p, &p, 10);
        for (; !(*p == '-' || *p >= '0' && *p <= '9') && *p; ++p)
            ;
    }

    free(buffer);
    *out_n = n;
    return ns;
}

static inline void rev(int *xs, int n) {
  int *l=xs,*r=&xs[n-1];
  while(l<r){int tmp=*l;*l++=*r;*r--=tmp;}
}

#define CHECK(PRNT, EXPR, VAL)                                                 \
    if (EXPR == VAL)                                                           \
        printf(PRNT, EXPR);                                                    \
    else                                                                       \
        printf("Assertion %s == %s failed\n", #EXPR, #VAL);
