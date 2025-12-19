#include <math.h>
#include "util.h"

#define MAX(x,y) ((x)>(y)?(x):(y))
typedef struct { double x,y,θ,d; } coord; // x, y, angle from reference point, distance to ref point
coord  c_new(double x, double y) { coord c; c.x = x; c.y = y; c.θ=0, c.d=0; return c; }
double c_dst(coord  c, coord  d) { return sqrt(pow((c.x-d.x),2) + pow((c.y-d.y),2)); }
double c_θ  (coord  c, coord  d) { double x=atan2(c.x-d.x,c.y-d.y); return x>0?2*3.14-x:fabs(x); }
#define ORIG c_new(0, 0)

typedef struct { coord c; double θ; double d; } A;

int c_cmp(void const *w, void const *v) {
  coord x=*(coord*)w, y=*(coord*)v;
  $(x.θ<y.θ, return -1)$$(x.θ>y.θ, return 1)$$(x.d>y.d, return -1)$$(x.d<y.d, return 1)E(return 0)
}

int c_cmp2(void const *w, void const *v) {
  coord x=*(coord*)w, y=*(coord*)v;
  $(x.θ<y.θ, return -1)$$(x.θ>y.θ, return 1)$$(x.d<y.d, return -1)$$(x.d>y.d, return 1)E(return 0)
}


int main() {
    sz r; char **inp=read_lines("../inputs/day10.txt",&r), *p=inp[0]; sz c; while(*p){++c;++p;}
    sz az=0; coord as[999]; DO2(c,r,$(inp[j][i]=='#',as[az++]=c_new(i,j))); DO(r,free(inp[i]));free(inp); // asteroid+size
    sz mx=0; coord mc=ORIG, cs[az]; // max, max coord, coords
    DO(az, sz m=0;
       DOj(az, cs[j]=as[j]; cs[j].d=c_dst(as[i],cs[j]); cs[j].θ=c_θ(as[i],cs[j])); // angle relative to as[i]
       qsort(&cs, az, sizeof(coord), c_cmp);                             // sort to find non-hidden asteroids
       double l=999; DO(az, $(l!=cs[i].θ,++m); l=cs[i].θ); $(mx<m, mx=m; mc=as[i]));
    CHECK(mx,340);

    // Do the same thing, but with respect to a fixed ref point and preferring close things. Banking on the fact that
    // we hit 200 asteroids before completing a full rotation (which we even do in the example).
    DO(az, cs[i]=as[i]; cs[i].θ=c_θ(mc,cs[i]); cs[i].d=c_dst(mc,cs[i])); qsort(&cs, az, sizeof(coord), c_cmp2);
    double l=999; sz n=0; DO(az, $(l!=cs[i].θ,++n); $(n==200,CHECK(2628,(sz)(cs[i].x*100+cs[i].y));break); l=cs[i].θ);
}
