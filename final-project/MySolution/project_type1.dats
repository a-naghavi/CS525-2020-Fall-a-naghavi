(* ****** ****** *)
#staload "./project.sats"
(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
(* ****** ****** *)
#staload _ =
"./../../mylib/mylib.dats"
#include "share/atspre_staload.hats"
(* ****** ****** *)
(* ****** ****** *)
(* ****** ****** *)
implement
T1Pnil = T1Pbas("nil")
implement
T1Pint = T1Pbas("int")
//
implement
T1Pbool = T1Pbas("bool")
//
implement
T1Pstring = T1Pbas("string")

(* ****** ****** *)

implement
fprint_val<type1> =  fprint_type1

(* ****** ****** *)

implement
type1_new_ext() =
T1Pext(tpext_new())

implement
type1_new_tup() =
T1Ptup(type1_new_ext() ,type1_new_ext())



local

fun
auxtype
( tp1: type1
, tp2: type1): bool =
(
case- tp1 of
|
T1Pbas(nm1) =>
(
  case+ tp2 of
  | T1Pbas(nm2) => (nm1 = nm2) | _ => false
)
|
T1Pfun(tp11, tp12) =>
(
  case+ tp2 of
  | T1Pfun(tp21, tp22) =>
    type1_unify(tp11, tp21) && type1_unify(tp12, tp22) | _ => false
)
|
T1Ptup(tp11, tp12) =>
(
  case+ tp2 of
  | T1Ptup(tp21, tp22) =>
    type1_unify(tp11, tp21) && type1_unify(tp12, tp22) | _ => false
)
) (* end of [auxtype] *)

in


implement
type1_unify(tp1, tp2) =
(
case+ tp1 of
|
T1Pext(X1) =>
(
case
X1.get() of
| myoptn_nil() => 
  true where
  {
    val () = X1.set(tp2)
  }
| myoptn_cons(tp1) => type1_unify(tp1, tp2)
)
| _(*non-T1Pext*) =>
(
case+ tp2 of
|
T1Pext(X2) =>
(
case
X2.get() of
| myoptn_nil() => 
  true where
  {
    val () = X2.set(tp1)
  }
| myoptn_cons(tp2) => type1_unify(tp1, tp2)
)
| _(* non-T1Pext *) => auxtype(tp1, tp2)
)
) (* end of [type1_unify] *)
end // end of [local]
(* ****** ****** *)
(* ****** ****** *)

implement
print_type1(tm) =
fprint_type1(stdout_ref, tm)

(* ****** ****** *)

implement
fprint_type1
(out, tp) =
(
case+ tp of
| T1Pbas(nm) =>
  fprint!(out, "T1Pbas(", nm, ")")
| T1Pfun(tp1, tp2) =>
  fprint!
  (out, "T1Pfun(", tp1, ", ", tp2, ")")
| T1Ptup(tp1, tp2) =>
  fprint!
  (out, "T1Ptup(", tp1, ", ", tp2, ")")
| T1Pext(X0) =>
  fprint!(out, "T1Pext(", X0.get(), ")")
)

(* ****** ****** *)

(* end of [project_type1.dats] *)
