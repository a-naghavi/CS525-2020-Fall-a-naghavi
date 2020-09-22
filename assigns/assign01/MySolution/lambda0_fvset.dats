(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#staload "./../../../mylib/mylib.sats"

#include "share/atspre_staload.hats"
(* ****** ****** *)

implement print_mylist<string>(xs)=
(
let
	fun print_myl(el:mylist(string)): void =
		case el of
		| mylist_nil() => println!("]")
		| mylist_cons(hd,tl) => 
		(
		print!(hd, " ,");
		print_myl(tl);
		)
in
	print!("[");
	print_myl(xs);
end
)

implement{a} mylist_sing (x0)=
(
	mylist_cons{a}(x0,mylist_nil{a}())
)

extern fun {a:t@ype} geq_val_val(x0: a, x1: a): bool // for testing if x0 and x1 are equal

implement geq_val_val<t0var>(x0, x1) = (x0 = x1) // just string comparison

implement geq_val_val<int>(x0, x1) = (x0 = x1) // just string comparison

implement{a} mylist_remove (xs,x0)=
(
	case xs of
	| mylist_nil() => mylist_nil()
	| mylist_cons(hd,tl) => 
	(
		if geq_val_val<a>(hd,x0) then 
			mylist_remove(tl,x0)
		else
			mylist_cons(hd,mylist_remove(tl,x0))
	)
)

implement{a} mylist_append (xs, ys)=
(
	case xs of
	| mylist_nil() => ys
	| mylist_cons(hd,tl) => mylist_cons{a}(hd,mylist_append(tl,ys))
)


(* ****** ****** *)

implement
t0erm_fvset(t0) =
(
case- t0 of
| T0Mint _ =>
  mylist_nil()
| T0Mvar(x0) =>
  mylist_sing(x0)
| T0Mapp(t1, t2) =>
  mylist_append
  (t0erm_fvset(t1), t0erm_fvset(t2))
| T0Mlam(x0, t1) =>
  mylist_remove(t0erm_fvset(t1), x0)
)

(* ****** ****** *)



(* end of [lambda0_fvset.dats] *)
