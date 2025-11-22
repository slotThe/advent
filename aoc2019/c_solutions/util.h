#pragma once

#include "m.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
