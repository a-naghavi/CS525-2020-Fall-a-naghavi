#include <stdio.h>
#include <stdlib.h>

#include "./../runtime.h"

/*
fun
fibo(n) =
if n >= 2 then fibo(n-1) + fact(n-2) else n
*/

lamval
fibo(lamval n)
{
  lamval tmp0;
  lamval tmp1;
  lamval tmp2;
  lamval tmp3;
  lamval tmp4;
  lamval tmp5;
  tmp0 = LAMOPR_igt(n, LAMVAL_int(1));
  if (((lamval_int)tmp0)->data) {
    tmp1 = LAMOPR_isub(n, LAMVAL_int(1));
    tmp2 = fibo(tmp1);
    tmp3 = LAMOPR_isub(n, LAMVAL_int(2));
    tmp4 = fibo(tmp3);
    tmp5 = LAMOPR_iadd(tmp2, tmp4);
  } else {
    tmp5 = n;
  }
  return tmp5;
}

int main() {
  lamval i10 = LAMVAL_int(10);
  LAMVAL_print(i10); printf("\n");
  printf("fibo(10) = "); LAMVAL_print(fibo(i10)); printf("\n");
  return 0 ;
}
