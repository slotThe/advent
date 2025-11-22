#include <math.h>
#include "util.h"

typedef struct { double x; double y; } coord;
coord c_new(double x, double y) { coord c; c.x = x; c.y = y; return c; }
coord c_add(coord c1, coord c2) { return c_new(c1.x + c2.x, c1.y + c2.y); }
coord c_sub(coord c1, coord c2) { return c_new(c1.x - c2.x, c1.y - c2.y); }
#define ORIG c_new(0, 0)

typedef struct { coord beg; coord end; } line;
line l_new(coord beg, coord end) { line l; l.beg = beg; l.end = end; return l; }
sz l_len(line a) { return fabs(a.beg.x==a.end.x?a.beg.y-a.end.y:a.beg.x-a.end.x); }

line expand(char d, int mag, coord *start) {
    coord o; S(d,Cs('R',o=c_new(1,0));Cs('L',o=c_new(-1,0));Cs('U',o=c_new(0,1));Cs('D',o=c_new(0,-1)));
    coord end=c_add(*start, c_new(o.x * mag, o.y * mag)); line l=l_new(*start, end); *start=end;
    return l;
}

coord isect(line a, line b) {
    double a₁ = a.end.x - a.beg.x, b₁ = -(b.end.x - b.beg.x),  c₁ = b.beg.x - a.beg.x;
    double a₂ = a.end.y - a.beg.y, b₂ = -(b.end.y - b.beg.y),  c₂ = b.beg.y - a.beg.y;
    // Solve the linear system Ax = c.
    double d = a₁*b₂ - a₂*b₁;
    if (d != 0) {
        double s=(c₁*b₂ - c₂*b₁)/d, t=(a₁*c₂ - a₂*c₁)/d;
        return (0 <= s && s <= 1 && 0 <= t && t <= 1) // solution within bounds?
             ? c_new(a.beg.x + s*(a.end.x-a.beg.x), a.beg.y + s*(a.end.y-a.beg.y))
             : ORIG;
    } else { return ORIG; }
}

int main() {
    sz _; char *buffer = readf("../inputs/day03.txt",&_); char *p=buffer; line a[999], b[999], *cur=a;
    int an=0, bn=0, k=0; coord c=ORIG;
    while (*p) {
        char d=*p++; int mag=strtol(p,&p,10); cur[k++]=expand(d,mag,&c);
        $(*p==',', p++)$$(*p=='\n'&&*++p, cur=b; an=k; k=0; c=ORIG) // second condition: second line
    }
    bn=k; free(buffer);

    double one = 10e6;
    DO2(an, bn, coord c=isect(a[i],b[j]); double mh=fabs(c.x)+fabs(c.y); $(mh>0 && mh<one, one=mh))
    CHECK((sz)one, 232);

    double two = 10e6;
    DO2(an, bn,
        _(coord c = isect(a[i], b[j]);
          if (!(c.x == 0 && c.y == 0)) {
              sz la=l_len(l_new(a[i].beg,c)); DO(i,la+=l_len(a[i]));
              sz lb=l_len(l_new(b[j].beg,c)); DO(j,lb+=l_len(b[i]));
              double mh=la+lb; $(mh>0 && mh<two, two=mh);
          }))
    CHECK((sz)two, 6084);
}
