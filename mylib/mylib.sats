(* ****** ****** *)

datatype
myoptn(a:t0ype) =
| myoptn_none of ()
| myoptn_some of (a)

(* ****** ****** *)
//
fun
{a:t0ype}
print_myoptn: myoptn(a) -> void
fun
{a:t0ype}
prerr_myoptn: myoptn(a) -> void
//
fun
{a:t0ype}
fprint_myoptn
(out: FILEref, xs: myoptn(a)): void
fun{}
fprint_myoptn$sep(out: FILEref): void
//
overload print with print_myoptn
overload prerr with prerr_myoptn
overload fprint with fprint_myoptn
//
(* ****** ****** *)

datatype
mylist(a:t0ype) =
| mylist_nil of ()
| mylist_cons of (a, mylist(a))

(* ****** ****** *)
//
fun
{a:t0ype}
print_mylist: mylist(a) -> void
fun
{a:t0ype}
prerr_mylist: mylist(a) -> void
//
fun
{a:t0ype}
fprint_mylist
(out: FILEref, xs: mylist(a)): void
fun{}
fprint_mylist$sep(out: FILEref): void
//
overload print with print_mylist
overload prerr with prerr_mylist
overload fprint with fprint_mylist
//
(* ****** ****** *)

(* end of [mylib.sats] *)
