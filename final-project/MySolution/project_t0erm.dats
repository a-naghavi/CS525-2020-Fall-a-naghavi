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
fprint_val<t0erm> = fprint_t0erm
implement
fprint_val<t0dcl> = fprint_t0dcl
implement
fprint_val<t0pgm> = fprint_t0pgm
implement
fprint_val<type0> =  fprint_type0
(* ****** ****** *)

implement
print_t0erm(tm) =
fprint_t0erm(stdout_ref, tm)
implement
print_t0dcl(tm) =
fprint_t0dcl(stdout_ref, tm)
implement
print_t0pgm(tm) =
fprint_t0pgm(stdout_ref, tm)

(* ****** ****** *)


implement
fprint_t0erm
(out, tm) =
(
case+ tm of
| T0Mnil() =>
  fprint!(out, "T0Mnil(", ")")
//
| T0Mbtf(x0) =>
  fprint!(out, "T0Mbtf(", x0, ")")
//
| T0Mint(x0) =>
  fprint!(out, "T0Mint(", x0, ")")
//
| T0Mflt(x0) =>
  fprint!(out, "T0Mflt(", x0, ")")
//
| T0Mstr(x0) =>
  fprint!(out, "T0Mstr(", x0, ")")
//
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
//
| T0Mlam(x0, tp1, tm2,tp2) =>
  fprint!
  ( out
  , "T0Mlam(", x0, "; ", tp1, "; ", tm2,"; ",tp2, ")")
| T0Mapp(tm1, tm2) =>
  fprint!(out, "T0Mapp(", tm1, "; ", tm2, ")")
//
| T0Mlet(tdl, tm1) =>
  fprint!
  ( out
  , "T0Mlet(; ", tm1, ")")
//
| T0Mfix(f0, tm2) =>
  fprint!
  ( out
  , "T0Mfix1(", f0, "; ", tm2, ")")
//
| T0Mprj(tup,i) =>
  fprint!(out, "T0Mprj(", tup, ",", i,")")
//
| T0Mtup(tm1) =>
  fprint!(out, "T0Mtup(", tm1, ")")
//
| T0Mopr1(opr, tm1) =>
  fprint!(out, "T0Mopr1(", opr, "; ", tm1, ")")
//
| T0Mopr2(opr, tm1, tm2) =>
  fprint!
  ( out
  , "T0Mopr2(", opr, "; ", tm1, "; ", tm2, ")")
//
| T0Moprs(opr, tms) =>
  fprint!(out, "T0Moprs(", opr, "; ", tms, ")")
//
| T0Manno(tm1, tp2) =>
  fprint!(out, "T0Manno(", tm1, "; ", tp2, ")")
//
| T0Mcond(tm1, tm2, tm3) =>
  fprint!
  ( out
  , "T0Mcond(", tm1, "; ", tm2, "; ", tm3, ")")
//
)

(* ****** ****** *)

implement
fprint_t0dcl
(out, tdcl) =
(
case+ tdcl of
| T0DCL(x0, def) =>
  fprint!
  (out, "T0DCL(", x0, ", ", def, ")")
)

implement
fprint_t0pgm
(out, tpgm) =
(
case+ tpgm of
| T0PGM(tdcl, tm) =>
  fprint!
  (out, "T0PGM(", tdcl, ", ", tm, ")")
)


(* ****** ****** *)
implement
print_value(v0) =
fprint_value(stdout_ref, v0)
(* ****** ****** *)

implement
fprint_value(out, v0) =
(
case+ v0 of
| VALnil _ =>
  fprint!(out, "VALnil")
| VALint(i0) =>
  fprint!(out, "VALint(", i0, ")")
| VALbtf(b0) =>
  fprint!(out, "VALbtf(", b0, ")")
| VALstr(s0) =>
  fprint!(out, "VALstr(", s0, ")")
|
  VALtup(v1,v2) =>
  fprint!(out, "VALtup(", v1, ", ", v2, ")")
| VALlam(t0, env) =>
  fprint!
  (out, "VALlam(", t0, "; ", "...", ")")
| VALfix(f0, vlam) =>
  fprint!(out, "VALfix(", f0, "; ", vlam, ")")

)
(* ****** ****** *)

(* end of [project_t0erm.dats] *)
