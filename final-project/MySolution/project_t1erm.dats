(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
(* ****** ****** *)
#staload _ =
"./../../mylib/mylib.dats"
(* ****** ****** *)
#staload "./project.sats"
(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
(* ****** ****** *)
#include "share/atspre_staload.hats"
(* ****** ****** *)

implement
fprint_val<t1erm> = fprint_t1erm
implement
fprint_val<t1dcl> = fprint_t1dcl
implement
fprint_val<t1pgm> = fprint_t1pgm
implement
fprint_val<type1> =  fprint_type1

implement
print_t1erm(tm) =
fprint_t1erm
(stdout_ref, tm)
implement
print_t1dcl(tdcl) =
fprint_t1dcl(stdout_ref, tdcl)
implement
print_t1pgm(tpgm) =
fprint_t1pgm(stdout_ref, tpgm)

(* ****** ****** *)

implement
fprint_t1erm
(out, tm) =
(
case+ tm of
| T1Mnil() =>
  fprint!(out, "T1Mnil(", ")")
//
| T1Mbtf(x0) =>
  fprint!(out, "T1Mbtf(", x0, ")")
//
| T1Mint(x0) =>
  fprint!(out, "T1Mint(", x0, ")")
//
| T1Mstr(x0) =>
  fprint!(out, "T1Mstr(", x0, ")")
//
| T1Mvar(x0) =>
  fprint!(out, "T1Mvar(", x0, ")")
//
| T1Mlam(x0, tp1, tm2) =>
  fprint!
  ( out
  , "T1Mlam(", x0, "; ", tp1, "; ", tm2,")")
| T1Mapp(tm1, tm2) =>
  fprint!(out, "T1Mapp(", tm1, "; ", tm2, ")")
//
| T1Mlet(tdl, tm1) =>
  fprint!
  ( out
  , "T1Mlet(",tdl,"; ", tm1, ")")
//
| T1Mfix(f0,tp1, tm1) =>
  fprint!
  ( out
  , "T1Mfix(", f0, "; ", tp1, "; ",tm1, ")")
//
| T1Mfst(tup) =>
  fprint!(out, "T1Mfst(", tup, ")")
//
| T1Msnd(tup) =>
  fprint!(out, "T1Msnd(", tup, ")")
//
| T1Mtup(tm1, tm2) =>
  fprint!(out, "T1Mtup(", tm1, "; ", tm2, ")")
//
| T1Mopr1(opr, tm1) =>
  fprint!(out, "T1Mopr1(", opr, "; ", tm1, ")")
//
| T1Mopr2(opr, tm1, tm2) =>
  fprint!
  ( out
  , "T1Mopr2(", opr, "; ", tm1, "; ", tm2, ")")
//
| T1Manno(tm1, tp2) =>
  fprint!(out, "T1Manno(", tm1, "; ", tp2, ")")
//
| T1Mcond(tm1, tm2, tm3) =>
  fprint!
  ( out
  , "T1Mcond(", tm1, "; ", tm2, "; ", tm3, ")")
//
)

(* ****** ****** *)
implement
fprint_t1dcl
(out, tdcl) =
(
case+ tdcl of
| T1DCL(x0, def) =>
  fprint!
  (out, "T1DCL(", x0, ", ", def, ")")
)

implement
fprint_t1pgm
(out, tpgm) =
(
case+ tpgm of
| T1PGM(tdcl, tm) =>
  fprint!
  (out, "T1PGM(", tdcl, ", ", tm, ")")
)
(* ****** ****** *)

(* end of [project_t1erm.dats] *)
