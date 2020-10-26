(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
#staload "./../midterm.sats"
(* ****** ****** *)

fun
tpext_new(): type0

(* ****** ****** *)
typedef
type0opt = myoptn(type0)
(* ****** ****** *)
fun
tpext_get
(X: tpext): type0opt
fun
tpext_set
(X: tpext, tp: type0): void
(* ****** ****** *)
fun
tpext_ref_eq
(X: tpext,Y: tpext): bool

overload .get with tpext_get
overload .set with tpext_set

(* ****** ****** *)

(*
fun
eq_type0_type0
(tp1: type0, tp2: type0): bool
overload = with eq_type0_type0
*)
fun
tunify
(tp1: type0, tp2: type0): bool

fun
t0erm_tinfer0
(prog: t0erm): type0
fun
t0erm_tinfer1
(t0m: t0erm, env: s0env): type0

fun
s0env_remove
(s0env, t0var): s0env