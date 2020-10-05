(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#staload "./../../../mylib/mylib.sats"
(* ****** ****** *)

implement fprint_mylist<t0erm>(out, xs): void =
(
let
  fun print_myl(el:mylist(t0erm)): void =
    case el of
    | mylist_nil() => fprint!(out,"]")
    | mylist_cons(hd,tl) => 
    (
    fprint!(out,hd, " ,");
    print_myl(tl);
    )
in
  fprint!(out,"[");
  print_myl(xs);
end
)

(******************************)



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
//
| T0Mbool(btf) =>
  fprint!
  ( out
  , "T0Mbool(", btf, ")")
| T0Mopr1(x0, t1) =>
  fprint!
  ( out
  , "T0Mopr1(", x0, "; ", t1, ")")
| T0Mopr2(x0, t1, t2) =>
  fprint!
  ( out
  , "T0Mopr2(", x0, "; ", t1, "; ", t2, ")")
//
(*
| T0Mfix0(t1) =>
  fprint!(out, "T0Mfix0(", t1, ")")
*)
| T0Mfix1(x1, t1) =>
  fprint!(out, "T0Mfix1(", x1, "; ", t1, ")")
//
| T0Mcond(t1, t2, t3) =>
  fprint!
  ( out
  , "T0Mcond(", t1, "; ", t2, "; ", t3, ")")
(***************************Implemented by Amin************************)
| T0Mtup(tup) =>
fprint!
  ( out
  , "T0Mtup(", tup, ")")

| T0Mprj(t,i) =>
  fprint!
  ( out
  , "T0Mprj(", t, ", ", i, ")")
//
(*********************End of implemented by Amin***********************)
)


(* end of [lambda0_print.dats] *)

