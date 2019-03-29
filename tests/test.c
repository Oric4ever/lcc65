static char gc,gc2,*gcp;
static int  gi,gi2,*gip;

void char_asgn_test()
{
    char a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z;
    char *lp, *lp1, *lp2, *lp3;
    // a and lp are used several times so that they go into registers

    gc   = 'A'; // ASGNB_CC
    a    = 'A'; // ASGNB_CC
    *lp  = 'A'; // ASGNB_CZ
    *gcp = 'A'; // ASGNB_CD
    b    = 'A'; // ASGNB_CA
    *lp1 = 'A'; // ASGNB_CY

    gc   = gc2; // ASGNB_DC
    a    = gc;  // ASGNB_DC
    *lp  = gc;  // ASGNB_DZ
    *gcp = gc;  // ASGNB_DD
    c    = gc;  // ASGNB_DA
    *lp2 = gc;  // ASGNB_DY

    gc   = d;   // ASGNB_YC
    a    = e;   // ASGNB_YC
    *lp  = f;   // ASGNB_YZ
    *gcp = g;   // ASGNB_YD
    h    = i;   // ASGNB_YA
    *lp3 = j;   // ASGNB_YY
}

void int_asgn_test()
{
    int a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z;
    int *lp, *lp1, *lp2, *lp3;
    // a and lp are used several times so that they go into registers

    gi   = 42 ; // ASGNW_CC
    a    = 42 ; // ASGNW_CC
    *lp  = 42 ; // ASGNW_CZ
    *gip = 42 ; // ASGNW_CD
    b    = 42 ; // ASGNW_CA
    *lp1 = 42 ; // ASGNW_CY

    gi   = gi2; // ASGNW_DC
    a    = gi;  // ASGNW_DC
    *lp  = gi;  // ASGNW_DZ
    *gip = gi;  // ASGNW_DD
    c    = gi;  // ASGNW_DA
    *lp2 = gi;  // ASGNW_DY

    gi   = d;   // ASGNW_YC
    a    = e;   // ASGNW_YC
    *lp  = f;   // ASGNW_YZ
    *gip = g;   // ASGNW_YD
    h    = i;   // ASGNW_YA
    *lp3 = j;   // ASGNW_YY
}

void cte_test()
{
    int a, b;
    b = a + 2;
    b = 2 + a;
    b = 2 - a;
    b = a - 2;
    b = 3 * a;
    b = a * 3;
    b = a / 3;
    b = 3 / a;
    b = a & 3;
    b = 3 & a;
}

void ptr_arith()
{
    char *a, *b;
    char c = *a;
}

int main(void)
{
    int a = 1, b;
    b = a + (int)&a;
    b = (int)&a + a;
    b = a -  (int)&a;
    b = (int)&a - a;
    b = a * (int)&a;
    b = (int)&a * a;
    b = (int)&a / a;
    b = a / (int)&a;
    b = a & (int)&a;
    b = (int)&a & a;
}
