#include "intcode.h"

// Generate next permutation: https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
sz gen_perm(sz *p) {
    sz k=-1;DO(4,$(p[i]<p[i+1],k=i));$(k==-1,return 0); // largest k w/ p[k]<p[k+1]
    sz l;DO(5-k,$(p[k]<p[k+i],l=k+i));                  // largest l>k w/ p[k]<p[l]
    sz tmp=p[k];p[k]=p[l];p[l]=tmp;
    l=k+1;sz r=4;while(l<r){tmp=p[l];p[l++]=p[r];p[r--]=tmp;}; // rev p[k+1]..p[n-1].
    return 1;
}

int main() {
    sz mx=0,n,*inp=read_ints("../inputs/day07.txt",&n), p[5]={0,1,2,3,4};

    do {
        sz o=0;
        DO(5, _(sz in[2]={p[i],o};IC*ic;ic=ic_new(inp,n,in,2);ic_execute(ic);o=lout;ic_kill(ic)));
        $(o>mx,mx=o);
    } while (gen_perm(p));
    CHECK(mx,18812);

    mx=0;
    IC ics[5]={}; sz q[5]={5,6,7,8,9},*o=M(1024),po=0;
    do {
        sz a=0,on=1,po=0;DO(1024,o[i]=0);
        DO(5,o[0]=q[i];ics[i]=*ic_new(inp,n,o,on)); o[0]=0;  // initialise
        while (1) {
            IC *ic=&ics[a]; ic_push_inp(ic,o,on);            // push new input
            EC r=ic_execute(ic);
            on=io[-1]; C(o,io,on); po=lout;                  // copy output
            DO(128,io[i]=0);io[-1]=0;                        // zero output
            $(a==4&&r==succ,DO(5,ic_free(&ics[i]));break)E(a=(a+1)%5);
        };
        $(po>mx,mx=po);
    } while (gen_perm(q));
    CHECK(mx, 25534964);
    free(o);free(inp);
}
