(* ****** ****** *)

#staload "./mylib.sats"

(* ****** ****** *)

#include
"share/atspre_staload.hats"
#include
"share/atspre_staload_libats_ML.hats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)
implement
{a}
print_myoptn(xs) = 
fprint_myoptn<a>(stdout_ref, xs)
implement
{a}
prerr_myoptn(xs) = 
fprint_myoptn<a>(stderr_ref, xs)
(* ****** ****** *)
implement
{a}
fprint_myoptn
  (out, xs) =
(
case+ xs of
|
myoptn_none() => 
fprint(out, "none()")
|
myoptn_some(x0) =>
(fprint(out, "some(");fprint_val<a>(out, x0);fprint(out, ")"))
) (* end of [fprint_myoptn] *)
(* ****** ****** *)

(* end of [mylib.dats] *)
