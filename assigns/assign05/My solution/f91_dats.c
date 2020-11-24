#include <stdio.h>
#include <stdlib.h>

#include "./../runtime.h"


// fun
// f91(x: int): int =
// if x > 100 then x - 10 else f91(f91(x+11))

// fun
// f91(x: int): int =
// if x > 100 then x - 10 else f91(r0,f91(r1,x+11))

// fun fx(x): //recursive
// if x<1 z else opr(n,fx(x-1))

// fun fx(r,x): //tail recursive
// if x<1 r else fx(opr(r,n),x-1)

// fx(z,x)
#if(0)
lamval
f91
( lamval x )
{
  lamval tmp0;
  lamval tmp1;
  lamval tmp2;
  lamval tmp3;
  tmp0 =
  LAMOPR_igt(x, LAMVAL_int(100));
  if (((lamval_int)tmp0)->data) {
    tmp3 = LAMOPR_isub(x, LAMVAL_int(10));

  } else {
    tmp1 = LAMOPR_iadd(x, LAMVAL_int(11));
    tmp2 = f91(tmp1);
    tmp3 = f91(tmp2);
  }

  return tmp3;
}
#endif

#if(1)
lamval
f91
( lamval x )
{
  lamval x2=x;
  lamval tmp0;
  lamval tmp1;
  lamval tmp2;
  lamval tmp3;
  do{
  tmp0 =LAMOPR_igt(x2, LAMVAL_int(100));
  if (((lamval_int)tmp0)->data) {
    tmp3 = LAMOPR_isub(x2, LAMVAL_int(10));

  } else {
    tmp1 = LAMOPR_iadd(x2, LAMVAL_int(11));
    x2 = f91(tmp1); 
    //tmp3 = f91(r,tmp2);
    continue;
  }
  break;
} while(1);
  return tmp3;
}
#endif


int main() {
  lamval x = LAMVAL_int(10);
  printf("x = ");LAMVAL_print(x); printf("\n");
  printf("f91(x) = "); LAMVAL_print(f91(x)); printf("\n");
  return 0 ;
}
