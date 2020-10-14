(* ****** ****** *)
#staload "./lambda2.sats"
(* ****** ****** *)
#staload "./../../../mylib/mylib.sats"
(* ****** ****** *)
#include "share/atspre_staload.hats"
(* ****** ****** *)

implement
print_t0erm(tm) =
fprint_t0erm
(stdout_ref, tm)

(* ****** ****** *)

implement
fprint_t0erm
(out, tm) =
(
case+ tm of
| T0Mint(i0) =>
  fprint!(out, "T0Mint(", i0, ")")
| T0Mbool(b0) =>
  fprint!(out, "T0Mbool(", b0, ")")
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x0,tp, t0) =>
  fprint!
  (out, "T0Mlam(", x0, "; ",tp,"; ", t0, ")")
| T0Mapp(t1, t2) =>
  fprint!
  (out, "T0Mapp(", t1, "; ", t2, ")")
//
| T0Mopr1(x0, t1) =>
  fprint!
  ( out
  , "T0Mopr1(", x0, "; ", t1, ")")
| T0Mopr2(x0, t1, t2) =>
  fprint!
  ( out
  , "T0Mopr2(", x0, "; ", t1, "; ", t2, ")")
//
| T0Mfix1(x1,tp, t1) =>
  fprint!(out, "T0Mfix1(", x1, "; ", tp, "; ", t1, ")")
//
| T0Mcond(t1, t2, t3) =>
  fprint!
  ( out
  , "T0Mcond(", t1, "; ", t2, "; ", t3, ")")
| T0Mtup(t1, t2) =>
  fprint!
  ( out
  , "T0Mtup(", t1, "; ", t2, ")")
| T0Mfst(t1) =>
  fprint!
  ( out
  , "T0Mfst(",t1, ")")
| T0Msnd(t1) =>
  fprint!
  ( out
  , "T0Msnd(", t1, ")")
| T0Manno(t1, tp) =>
  fprint!
  (out, "T0Manno(", t1, "; ", tp, ")")  
//

)

(* ****** ****** *)

(* end of [lambda2_term.dats] *)