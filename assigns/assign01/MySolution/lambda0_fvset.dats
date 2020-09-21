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

val l0=mylist_nil{string}()
val l1=mylist_cons{string}("a",l0)
val l2=mylist_cons{string}("b",l1)
val l3=mylist_cons{string}("c",l2)
val l4=mylist_cons{string}("d",l3)
val S =
T0Mlam("x",
T0Mlam("y", 
T0Mlam("z",
T0Mapp(
T0Mapp(T0Mvar("x"), T0Mvar("z"))
,
T0Mapp(T0Mvar("y"), T0Mvar("z"))
)
)
)
)
val sl=t0erm_fvset(S)
val () = println!("\n*** Testing mylist_append and mylist_remove: ")
val ()=print!("l1 = ",l4)
val ()=print!("l2 = ",l3)
val ()=print!("append(l1,l2) = ", mylist_append(l4,l3))
val ()=print!("remove(l1,'b') = ", mylist_remove(l4,"b"))
val ()=println!("T1 = ",S)
val ()=println!("fvset(T1) = ",t0erm_fvset(S))

(* end of [lambda0_fvset.dats] *)
