(* ****** ****** *)
#staload "./project.sats"
(* ****** ****** *)
#staload "./../../mylib/mylib.sats"
(* ****** ****** *)
#include "share/atspre_staload.hats"
(* ****** ****** *)
val
the_envir_nil =
D1ENV(mylist_nil())



extern fun
t1erm_interp1
(t1m0:t1erm, env0:d1env):value
extern fun 
tdclist_interp1
(tdl0:t1dclist,env0:d1env):d1env
extern fun
tdclist_interp
(tdl0:t1dclist):d1env





(* ****** ****** *)
extern fun
t1erm_interp(prog:t1erm):value
implement
t1erm_interp(prog) =
t1erm_interp1(prog, the_envir_nil)

(* ****** ****** *)

extern
fun
envir_search
(d1env, t1var): myoptn(value)
implement
envir_search
(env, x0) =
(
  auxlst(xvs)
) where
{
val+
D1ENV(xvs) = env
fun
auxlst
( xvs
: mylist
  @(t1var, value)
) : myoptn(value) =
(
case+ xvs of
| mylist_nil() =>
  myoptn_nil()
| mylist_cons(xv1, xvs) =>
  if t1var_get_name(x0) = t1var_get_name(xv1.0)
  then myoptn_cons(xv1.1) else auxlst(xvs)
)
} (*where*) // end of [envir_search]

(* ****** ****** *)

extern
fun
envir_extend
(d1env, t1var, value): d1env
implement
envir_extend
(env, x0, v0) =
let
val+
D1ENV(xvs) = env
in
D1ENV(mylist_cons((x0, v0), xvs))
end // end of [envir_extend]

(* ****** ****** *)

local

fun
aux_var
( t1m0: t1erm
, env0: d1env): value =
let
//
(*
val () =
println!
("aux_var: t1m0 = ", t1m0)
*)
//
val-T1Mvar(x0) = t1m0
val
opt = envir_search(env0, x0)
//
in
case- opt of myoptn_cons(v0) => v0
end // end of [aux_var]

(* ****** ****** *)

fun
aux_app
( t1m0: t1erm
, env0: d1env): value =
let
//
val-
T1Mapp
(tfun, targ) = t1m0
//
val vfun =
t1erm_interp1(tfun, env0)
val varg =
t1erm_interp1(targ, env0)
//
in
case- vfun of
|
VALlam(tlam, elam) =>
let
val-
T1Mlam(x0,_, body) = tlam
in
  t1erm_interp1
  ( body
  , envir_extend(elam, x0, varg))
end
|
VALfix(f0, vlam) =>
let
val-
VALlam(t0, elam) = vlam
val-
T1Mlam(x0,_, body) = t0
val efix =
envir_extend(elam, f0, vfun)
in
  t1erm_interp1
  ( body
  , envir_extend(efix, x0, varg))
end
  
end // end of [aux_app]

(* ****** ****** *)

fun
aux_cond
( t1m0: t1erm
, env0: d1env): value =
let
//
val-
T1Mcond
(t1, t2, t3) = t1m0
//
in
let
val v1 =
t1erm_interp1(t1, env0)
in
case- v1 of
|
VALbtf(b1) =>
(
  if b1
  then t1erm_interp1(t2, env0)
  else t1erm_interp1(t3, env0)
)
end(*let*)
end(*let*) // end of [aux_cond]

(* ****** ****** *)

fun
aux_opr1
( t1m0: t1erm
, env0: d1env): value =
let
//
val-
T1Mopr1
(opr, t1) = t1m0
in
//
let
val v1 = t1erm_interp1(t1, env0)
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
( t1m0: t1erm
, env0: d1env): value =
let
//
val-
T1Mopr2
(opr, t1, t2) = t1m0
in
//
let
val v1 = t1erm_interp1(t1, env0)
val v2 = t1erm_interp1(t2, env0)
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
    ("There is an error in t1erm_interp1")
  }
//
end (*let*) end (*let*) // end of [aux_opr2]

in (* in-of-local *)


implement
t1erm_interp1
(t1m0, env0) =
(
case+ t1m0 of
//
| T1Mnil() =>
  VALnil()
| T1Mbtf(i0) =>
  VALbtf(i0)
| T1Mint(i0) =>
  VALint(i0)
| T1Mstr(s0) =>
  VALstr(s0)
| T1Mlam _ =>
  VALlam(t1m0, env0)

| T1Mlet(tdl0, body) =>
  let
    val new_env=tdclist_interp1(tdl0,env0)
  in
    t1erm_interp1(body,new_env)
  end
| T1Mvar _ =>
  aux_var(t1m0, env0)
//
| T1Mapp _ =>
  aux_app(t1m0, env0)
//
| T1Mfix(f0,_, t1m1) =>
  (
    VALfix(f0, vlam)
  ) where
  {
    val
    vlam = VALlam(t1m1, env0)
  }
//
| T1Mcond _ =>
  aux_cond(t1m0, env0)
//
| T1Mopr1 _ =>
  aux_opr1(t1m0,env0)
| T1Mopr2 _ =>
  aux_opr2(t1m0, env0)
| T1Mfst(t0) =>
(
  let
    val tv0=t1erm_interp1(t0,env0)
  in
  case+ tv0 of
  | VALtup(t1,t2) =>
    t1
  | _ =>
    let 
      val()=println!("There is an error in T1Mfst ,",t0)
      val () = assertloc(false)
     in 
      exit(1)
     end
  end
)
| T1Msnd(t0) =>
(
  let
    val tv0=t1erm_interp1(t0,env0)
  in
  case+ tv0 of
  | VALtup(t1,t2) =>
    t2
  | _ =>
    let 
      val()=println!("There is an error in T1Msnd")
      val () = assertloc(false)
    in 
      exit(1)
    end
  end
)
| T1Mtup(t1, t2) =>
VALtup(t1erm_interp1(t1,env0),t1erm_interp1(t2,env0))
//
| T1Manno(t0, _) =>
t1erm_interp1(t0,env0)

)

end // end of [local]


implement tdclist_interp(tdl0)=
tdclist_interp1(tdl0, the_envir_nil)

implement
tdclist_interp1
(tdl0, env0) =
(
  case tdl0 of 
  | mylist_cons(td0,tail) =>
    let
      val T1DCL(x,t0)=td0
      val t0val=t1erm_interp1(t0,env0)
    in
      tdclist_interp1(tail,envir_extend(env0,x,t0val))
    end
  | mylist_nil() =>
      env0
)


implement t1pgm_interp(pg0) =
(
  let
    val T1PGM(declist, t1t) = pg0  
  in
    t1erm_interp1(t1t,tdclist_interp(declist))
  end
)

(* ****** ****** *)

(* end of [lambda1_interp.dats] *)
