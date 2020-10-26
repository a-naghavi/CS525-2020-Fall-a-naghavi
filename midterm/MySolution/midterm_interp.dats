(* ****** ****** *)
#staload "./../midterm.sats"
(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
(* ****** ****** *)
#include "share/atspre_staload.hats"
(* ****** ****** *)
val
the_envir_nil =
D0ENV(mylist_nil())

(* ****** ****** *)

implement
t0erm_interp0(prog) =
t0erm_interp1(prog, the_envir_nil)

(* ****** ****** *)

extern
fun
envir_search
(d0env, t0var): myoptn(value)
implement
envir_search
(env, x0) =
(
  auxlst(xvs)
) where
{
val+
D0ENV(xvs) = env
fun
auxlst
( xvs
: mylist
  @(t0var, value)
) : myoptn(value) =
(
case+ xvs of
| mylist_nil() =>
  myoptn_nil()
| mylist_cons(xv1, xvs) =>
  if x0 = xv1.0
  then myoptn_cons(xv1.1) else auxlst(xvs)
)
} (*where*) // end of [envir_search]

(* ****** ****** *)

extern
fun
envir_extend
(d0env, t0var, value): d0env
implement
envir_extend
(env, x0, v0) =
let
val+
D0ENV(xvs) = env
in
D0ENV(mylist_cons((x0, v0), xvs))
end // end of [envir_extend]

(* ****** ****** *)

local

fun
aux_var
( t0m0: t0erm
, env0: d0env): value =
let
//
(*
val () =
println!
("aux_var: t0m0 = ", t0m0)
*)
//
val-T0Mvar(x0) = t0m0
val
opt = envir_search(env0, x0)
//
in
case- opt of myoptn_cons(v0) => v0
end // end of [aux_var]

(* ****** ****** *)

fun
aux_app
( t0m0: t0erm
, env0: d0env): value =
let
//
val-
T0Mapp
(tfun, targ) = t0m0
//
val vfun =
t0erm_interp1(tfun, env0)
val varg =
t0erm_interp1(targ, env0)
//
in
case- vfun of
|
VALlam(tlam, elam) =>
let
val-
T0Mlam(x0,_, body) = tlam
in
  t0erm_interp1
  ( body
  , envir_extend(elam, x0, varg))
end
|
VALfix(f0, vlam) =>
let
val-
VALlam(t0, elam) = vlam
val-
T0Mlam(x0,_, body) = t0
val efix =
envir_extend(elam, f0, vfun)
in
  t0erm_interp1
  ( body
  , envir_extend(efix, x0, varg))
end
  
end // end of [aux_app]

(* ****** ****** *)

fun
aux_cond
( t0m0: t0erm
, env0: d0env): value =
let
//
val-
T0Mcond
(t1, t2, t3) = t0m0
//
in
let
val v1 =
t0erm_interp1(t1, env0)
val t3_new=(
case+ t3 of
| myoptn_nil() =>
T0Mnil()
| myoptn_cons(tt) =>
tt
)
in
case- v1 of
|
VALbtf(b1) =>
(
  if b1
  then t0erm_interp1(t2, env0)
  else t0erm_interp1(t3_new, env0)
)
end(*let*)
end(*let*) // end of [aux_cond]

(* ****** ****** *)

fun
aux_opr1
( t0m0: t0erm
, env0: d0env): value =
let
//
val-
T0Mopr1
(opr, t1) = t0m0
in
//
let
val v1 = t0erm_interp1(t1, env0)
in
case+ opr of
| "-" =>
(
case- v1 of
| VALint(i1) => VALint(~i1)
)

| "~" =>
(
case- v1 of
| VALint(i1) => VALint(~i1)
| VALbtf(b1) => VALbtf(~b1)
)
//
| "print" =>
(
case- v1 of
| VALint(i1) =>
  let
  val () = print(i1) in VALnil()
  end
| VALbtf(b1) =>
  let
  val () = print(b1) in VALnil()
  end
| VALstr(s1) =>
  let
  val () = print(s1) in VALnil()
  end
  )
| _ (* else *) =>
  let
    val () =
    println!
    ("There is an error")
    val () = assertloc(false) in exit(1)
    end
  end (*let*)
end (*let*) // end of [aux_opr2]

(* ****** ****** *)

fun
aux_opr2
( t0m0: t0erm
, env0: d0env): value =
let
//
val-
T0Mopr2
(opr, t1, t2) = t0m0
in
//
let
val v1 = t0erm_interp1(t1, env0)
val v2 = t0erm_interp1(t2, env0)
in
case+ opr of
| "print" =>
(
case- v1 of
| VALint(i1) =>
  let
  val () = print(i1) in VALnil()
  end
| VALbtf(b1) =>
  let
  val () = print(b1) in VALnil()
  end
| VALstr(s1) =>
  let
  val () = print(s1) in VALnil()
  end
)
| "+" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALint(i1 + i2)
  end
| "-" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALint(i1 - i2)
  end
| "*" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALint(i1 * i2)
  end
| "/" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALint(i1 / i2)
  end
| "%" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALint(i1 - (i1/i2)*i2)
  end
//
| ">" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 > i2)
  end
| "<" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 < i2)
  end
| "=" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 = i2)
  end
| "<=" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 <= i2)
  end
| ">=" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 >= i2)
  end
| "!=" =>
  let
  val-VALint(i1) = v1
  val-VALint(i2) = v2 in VALbtf(i1 != i2)
  end
//
| _ (* else *) =>
  let
  val () = assertloc(false) in exit(1)
  end where
  {
    val () =
    println!
    ("There is an error in t0erm_interp1")
  }
//
end (*let*) end (*let*) // end of [aux_opr2]

in (* in-of-local *)

implement
t0erm_interp1
(t0m0, env0) =
(
case+ t0m0 of
//
| T0Mnil() =>
  VALnil()
| T0Mint(i0) =>
  VALint(i0)
| T0Mstr(s0) =>
  VALstr(s0)
| T0Mlam _ =>
  VALlam(t0m0, env0)

| T0Mlet(tv, t1, body) =>
  let
    val varg=t0erm_interp1(t1,env0)
  in
    t0erm_interp1(body,envir_extend(env0, tv, varg))
  end
| T0Mvar _ =>
  aux_var(t0m0, env0)
//
| T0Mapp _ =>
  aux_app(t0m0, env0)
//
| T0Mfix1(f0,_, t0m1) =>
  (
    VALfix(f0, vlam)
  ) where
  {
    val
    vlam = VALlam(t0m1, env0)
  }
//
| T0Mcond _ =>
  aux_cond(t0m0, env0)
//
| T0Mopr1 _ =>
  aux_opr1(t0m0,env0)
| T0Mopr2 _ =>
  aux_opr2(t0m0, env0)
| T0Mfst(t0) =>
(
  let
    val tv0=t0erm_interp1(t0,env0)
  in
  case+ tv0 of
  | VALtup(t1,t2) =>
    t1
  | _ =>
    let 
      val()=println!("There is an error in T0Mfst ,",t0)
      val () = assertloc(false)
     in 
      exit(1)
     end
  end
)
| T0Msnd(t0) =>
(
  let
    val tv0=t0erm_interp1(t0,env0)
  in
  case+ tv0 of
  | VALtup(t1,t2) =>
    t2
  | _ =>
    let 
      val()=println!("There is an error in T0Msnd")
      val () = assertloc(false)
    in 
      exit(1)
    end
  end
)
| T0Mtup(t1, t2) =>
VALtup(t0erm_interp1(t1,env0),t0erm_interp1(t2,env0))
//
| T0Manno(t0, _) =>
t0erm_interp1(t0,env0)

)

end // end of [local]

(* ****** ****** *)

(* end of [lambda1_interp.dats] *)
