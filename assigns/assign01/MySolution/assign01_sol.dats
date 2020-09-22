(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.sats"
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)
#staload "./lambda0_print.dats"
#staload "./lambda0_fvset.dats"
#staload "./lambda0_interp.dats"

#dynload "./lambda0_print.dats"
#dynload "./lambda0_fvset.dats"
#dynload "./lambda0_interp.dats"


(* ****** ****** *)

implement
t0erm_size(t0) =
( // not-tail-recursive
case+ t0 of
| T0Mint _ => 1
| T0Mvar _ => 1
| T0Mbtf _ => 1
| T0Mlam(_, t1) =>
  1 + t0erm_size(t1)
| T0Mapp(t1, t2) =>
  1 + t0erm_size(t1) + t0erm_size(t2)
| T0Mopr2(opr, t1, t2) =>
  1 + t0erm_size(t1) + t0erm_size(t2)
)

(* ****** ****** *)

(*
// concrete syntax
K == lam x => lam y => y
*)
val K =
T0Mlam("x", T0Mlam("y", T0Mvar("x")))

(* ****** ****** *)

(*
// concrete syntax
S ==
lam x =>
lam y => lam z => (x(z))(y(z))
*)
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

(* ****** ****** *)




implement
main0() =
{
(*Testing for Q1*)

val l0=mylist_nil{string}()
val l1=mylist_cons{string}("a",l0)
val l2=mylist_cons{string}("b",l1)
val l3=mylist_cons{string}("c",l2)
val l4=mylist_cons{string}("d",l3)
val S =
T0Mlam("x",
T0Mlam("y",
T0Mapp(
T0Mapp(T0Mvar("f"), T0Mvar("y"))
,
T0Mapp(T0Mvar("x"), T0Mvar("z"))
)
)
)

val ()=println!("\n*** Testing mylist_sing, mylist_append, mylist_remove, and t0erm_fvset (Q1): ")
val ()=print!("sing('x') = ",mylist_sing<string>("x"))
val ()=print!("l1 = ",l4)
val ()=print!("l2 = ",l3)
val ()=print!("append(l1,l2) = ", mylist_append(l4,l3))
val ()=print!("remove(l1,'b') = ", mylist_remove(l4,"b"))
val ()=println!("fvset(",S,") = ",t0erm_fvset(S))


(* Testing for Q2, and Q3 *)

val t1=T0Mopr2("<",T0Mint(2),T0Mint(3))
val t12=T0Mopr2("<=",T0Mint(5),T0Mint(5))
val t2=T0Mopr2(">",T0Mint(2),T0Mint(5))
val t22=T0Mopr2(">=",T0Mint(5),T0Mint(3))
val t3=T0Mopr2("!=",T0Mint(3),T0Mint(3))
val t4=T0Mopr2("==",T0Mint(3),T0Mint(3))
val () = println!("\n*** Testing t0erm_interp and t0_print for boolean terms and operations (Q2 , Q3): ")
val () = println!(t1 , " = ", t0erm_interp(t1))
val () = println!(t12 , " = ", t0erm_interp(t12))
val () = println!(t2 , " = ", t0erm_interp(t2))
val () = println!(t22 , " = ", t0erm_interp(t22))
val () = println!(t3 , " = ", t0erm_interp(t3))
val () = println!(t4 , " = ", t0erm_interp(t4))

val() = println!("\n\n*** end of testing ***")

} (* end of [main0] *)

(* ****** ****** *)

(* end of [lambda0.dats] *)