(* ****** ****** *)
#staload "./midterm_more.sats"
#staload "./../midterm.sats"
(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
(* ****** ****** *)
#include "share/atspre_staload.hats"
(* ****** ****** *)

implement
s0env_extend
(env, x0, tp) =
let
val+
S0ENV(xts) = env
in
S0ENV(mylist_cons((x0, tp), xts))
end // end of [envir_extend]

(* ****** ****** *)

implement
s0env_search
(env, x0) =
(
  auxlst(xts)
) where
{
val+
S0ENV(xts) = env
fun
auxlst
( xts
: mylist
  @(t0var, type0)
) : myoptn(type0) =
(
case+ xts of
| mylist_nil() =>
  myoptn_nil()
| mylist_cons(xt1, xts) =>
  if x0 = xt1.0
  then myoptn_cons(xt1.1) else auxlst(xts)
)
} (*where*) // end of [s0env_search]


implement s0env_remove (env,x0)=
(
let
	val+
S0ENV(xts) = env
fun
mylist_remove
( xts
: mylist
  @(t0var, type0)
, x0:t0var
) : mylist
  @(t0var, type0) =
(
	case xts of
	| mylist_nil() => mylist_nil()
	| mylist_cons(hd,tl) => 
	(
		if hd.0=x0 then 
			mylist_remove(tl,x0)
		else
			mylist_cons(hd,mylist_remove(tl,x0))
	)
)
in
	S0ENV(mylist_remove(xts,x0))
end
)

(* ****** ****** *)

(* end of [lambda2_s0env.dats] *)
