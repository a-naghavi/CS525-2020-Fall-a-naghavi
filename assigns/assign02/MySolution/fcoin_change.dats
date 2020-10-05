#include "share/atspre_staload.hats"
typedef
int4 = (int, int, int, int)
//
val theCoins = (1, 5, 10, 25): int4
//
extern
fun fcoin_get(n: int): int
extern
fun fcoin_change(sum: int): int

implement fcoin_get(n)=
(
  if n = 0 then theCoins.0
  else if n = 1 then theCoins.1
  else if n = 2 then theCoins.2
  else if n = 3 then theCoins.3
  else ~1 (* erroneous value *)
) (* end of [coin_get] *)
//
implement fcoin_change(sum) =
let
  fun aux (sum: int, n: int): int =
    if sum > 0 then
     (if n >= 0 then aux (sum, n-1) + aux (sum-fcoin_get(n), n) else 0)
    else (if sum < 0 then 0 else 1)
  // end of [aux]
in
  aux (sum, 3)
end // end of [coin_change]
//