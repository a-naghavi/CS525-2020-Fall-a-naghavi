(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#staload "./../../../mylib/mylib.sats"
(* ****** ****** *)
implement
print_t0erm(t0) =
fprint_t0erm(stdout_ref, t0)
(* ****** ****** *)
implement
fprint_t0erm(out, t0) =
(
case+ t0 of
| T0Mint(i0) =>
  fprint!(out, "T0Mint(", i0, ")")
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x0, t0) =>
  fprint!
  (out, "T0Mlam(", x0, "; ", t0, ")")
| T0Mapp(t1, t2) =>
  fprint!
  (out, "T0Mapp(", t1, "; ", t2, ")")
| T0Mopr2(x0, t1, t2) =>
  fprint!
  ( out
  , "T0Mopr2(", x0, "; ", t1, "; ", t2, ")")
| T0Mbtf(b0) =>
  fprint!(out, "T0Mbool(", b0, ")")
)

(* ****** ****** *)

(* ****** ****** *)

(* end of [lambda0_print.dats] *)
