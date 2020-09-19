(* ****** ****** *)

#include
"share/atspre_staload.hats"

(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
#staload "./../../mylib/mylib.dats"
(* ****** ****** *)
(*

variables: x, y, z, ... // names
abstractions: lam x.t // functions
applications: t1( t2 ) // function calls

*)

(* ****** ****** *)
typedef t0var = string
(* ****** ****** *)
//
// abstract syntax
//
datatype t0erm = // level-0
| T0Mint of (int)
| T0Mvar of t0var
| T0Mlam of (t0var, t0erm)
| T0Mapp of (t0erm, t0erm)

(* ****** ****** *)

extern
fun
print_t0erm
(xs: t0erm): void
overload print with print_t0erm

extern
fun
fprint_t0erm
(out: FILEref, xs: t0erm): void
overload fprint with fprint_t0erm

implement
print_t0erm(t0) =
fprint_t0erm(stdout_ref, t0)

implement
fprint_t0erm(out, t0) =
(
case+ t0 of
| T0Mint(i0) =>
  fprint!(out, "T0Mint(", i0, ")")
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x0, t0) =>
  fprint!(out, "T0Mlam(", x0, "; ", t0, ")")
| T0Mapp(t1, t2) =>
  fprint!(out, "T0Mapp(", t1, "; ", t2, ")")
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

extern
fun
t0erm_size(t0erm): int

implement
t0erm_size(t0) =
( // not-tail-recursive
case+ t0 of
| T0Mint _ => 1
| T0Mvar _ => 1
| T0Mlam(_, t1) =>
  1 + t0erm_size(t1)
| T0Mapp(t1, t2) =>
  1 + t0erm_size(t1) + t0erm_size(t2)
)

(* ****** ****** *)

extern
fun
t0erm_subst
( t0: t0erm
, x0: t0var
, sub: t0erm): t0erm

(* ****** ****** *)

implement
t0erm_subst
(t0, x0, sub) =
( // not-tail-recursive
case+ t0 of
//
| T0Mint _ => t0
//
| T0Mvar(x1) =>
  if
  (x0 = x1)
  then sub else t0
//
| T0Mlam(x1, t1) =>
  if (x0 = x1)
  then t0
  else
  T0Mlam
  ( x1
  , t0erm_subst(t1, x0, sub))
//
| T0Mapp(t1, t2) =>
  T0Mapp
  ( t0erm_subst(t1, x0, sub)
  , t0erm_subst(t2, x0, sub))
)

(* ****** ****** *)

implement
main0() =
{
val () =
println!
("Hello from [lambda0]!")
//
val () =
println!("size(K) = ", t0erm_size(K))
//
val () =
println!("size(S) = ", t0erm_size(S))
//
val () =
let
val x0 = "x"
val t0 = T0Mapp(T0Mvar(x0), T0Mvar(x0))
val sub = T0Mint(5)
in
println!
("subst(", t0, "; ", x0, "; ", sub, ") = ", t0erm_subst(t0, x0, sub))
end
//
}

(* ****** ****** *)

(* end of [lambda0.dats] *)
