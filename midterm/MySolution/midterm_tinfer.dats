(* ****** ****** *)
#staload "./../midterm.sats"
#staload "./midterm_more.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)

val
s0env_nil =
S0ENV(mylist_nil())

(* ****** ****** *)

extern fun
t0erm_tinfer0
(prog: t0erm): type0

extern fun
t0erm_tinfer1
(t0m: t0erm, env: s0env): type0

(* ***** ******* *)
implement
t0erm_tinfer0(prog) =
t0erm_tinfer1(prog, s0env_nil)

(* ****** ****** *)

extern fun remove_from_env(tm1:t0erm,env: s0env):s0env

implement remove_from_env(tm0,env)=
(
  case- tm0 of 
  |
  T0Mvar(x0) =>
    s0env_remove(env,x0)
  | _ => env
)

implement
t0erm_tinfer1
  (tm0, env) =
let
//
fun
tinfer1
(tm: t0erm): type0 =
t0erm_tinfer1(tm, env)
//
in
case- tm0 of
| 
T0Mnil _ => T0Pnil
//
|
T0Mint(_) => T0Pint
//
|
T0Mstr(_) => T0Pstring
//
|
T0Mvar(x0) =>
let
val
opt = s0env_search(env, x0)
//
in
case- opt of 
| myoptn_cons(tp) =>
 tp

end // end of [T0Mvar]
//
| T0Mopr1(opr, tm1) =>
let
val tp1 = tinfer1(tm1)
in
  case+ opr of
  | "print" =>
  (
  case- tp1 of
  | T0Pbas(nm1)
    when nm1 = "nil" => T0Pnil
  | T0Pbas(nm1)
    when nm1 = "int" => T0Pnil
  | T0Pbas(nm1)
    when nm1 = "bool" => T0Pnil
  | T0Pbas(nm1)
    when nm1 = "string" => T0Pnil
  )
  | _ =>
    tp1
end
| T0Mopr2(opr, tm1,tm2) =>
  let 
    val tp1=tinfer1(tm1)
    val tp2=tinfer1(tm2)
    val-true = tunify(tp1,T0Pint)
    val-true = tunify(tp2,T0Pint)
  in
    case+ opr of
      | ">" =>
        T0Pbool
      | "<" =>
        T0Pbool
      | "=" =>
        T0Pbool
      | "<=" =>
        T0Pbool
      | ">=" =>
        T0Pbool
      | "==" =>
        T0Pbool
      | "==" =>
        T0Pbool
      | "!=" =>
        T0Pbool
      | _ (* else *) =>
        tp1
  end
| T0Mfst(tm1) =>
  let
    val tpx=tinfer1(tm1)
//    val-true = tunify(T0Ptup(tpext_new(),tpext_new()),tp1)
//    val ()=println!("\n\n\n\n\n\n",tpx," ---- ",tm0,"\n\n\n\n\n\n")

  in
    case+ tpx of 
    | T0Pext(tex) =>
    (
    let
      val tp1=tpext_new()
      val tp2=tpext_new()
      val-true = tunify(T0Ptup(tp1,tp2),tpx)
    in
      tp1
    end
    )
    | _ =>
    let
      val-T0Ptup(tp1,tp2)=tpx
    in
      tp1
    end
  end
| T0Msnd(tm1) =>
  let
    val tpx=tinfer1(tm1)
//    val-true = tunify(T0Ptup(tpext_new(),tpext_new()),tp1)
//    val ()=println!("\n\n\n\n\n\n",tpx," ---- ",tm0,"\n\n\n\n\n\n")

  in
    case+ tpx of 
    | T0Pext(tex) =>
    (
    let
      val tp1=tpext_new()
      val tp2=tpext_new()
      val-true = tunify(T0Ptup(tp1,tp2),tpx)
    in
      tp2
    end
    )
    | _ =>
    let
      val-T0Ptup(tp1,tp2)=tpx
    in
      tp2
    end
  end
|
T0Mtup(tm1, tm2) =>
(
T0Ptup(tp1, tp2)
) where
{
  val tp1 = tinfer1(tm1)
  val tp2 = tinfer1(tm2)
}
//
|
T0Mlam
(x0, to1, tm2) =>
let
  val
  tp1 =
  (
  case+ to1 of
  | myoptn_nil() =>
    tpext_new()
  | myoptn_cons(tp1) => tp1
  ) : type0 // end-of-val
  val
  env = s0env_extend(env, x0, tp1)
  val tp2 = t0erm_tinfer1(tm2, env)
in
  T0Pfun(tp1, tp2)
end
//
|
T0Mapp(tm1, tm2) =>
let

  val tp1 = tinfer1(tm1)
  val new_env = remove_from_env(tm1,env)(* for occurs check *)
  val tp2 = t0erm_tinfer1(tm2,new_env)
in
  case+ tp1 of
  | T0Pext(tex) =>
    let
      val targ=tpext_new()
      val tres=tpext_new()
      val-true = tunify(T0Pfun(targ,tres),tp1)
    in
      tres
    end
  | _ =>
    let
      val-T0Pfun(targ, tres)=tp1
      val-true = tunify(targ, tp2)
    in
      tres
    end
end
//
|
T0Manno(tm1, tp2) =>
let
  val tp1 = tinfer1(tm1)
(*
  val ( ) = println!("tp1 = ", tp1)
  val ( ) = println!("tp2 = ", tp2)
*)
  val-true = tunify(tp1, tp2)
in
  tp2
end
|
T0Mcond(tmc, tm1,otm2) =>
let
  val tpc=tinfer1(tmc)
  val tp1=tinfer1(tm1)
  val new_env = remove_from_env(tm1,env)(* for occurs check *)
  val-true = tunify(tpc, T0Pbool)
in
  case otm2 of
  | myoptn_nil() =>
    tp1
  | myoptn_cons(tm2) =>
    let
    val tp2=tinfer1(tm1)
    val-true = tunify(tp1, tp2)
    in
      tp2
    end
end
| T0Mfix1(x0, to1, tm2) =>
let
  val
  tp1 =
  (
  case+ to1 of
  | myoptn_nil() =>
    tpext_new()
  | myoptn_cons(tp1) => tp1
  ) : type0 // end-of-val
  val
  env = s0env_extend(env, x0, tp1)
  val tp2 = t0erm_tinfer1(tm2, env)
in
  tp2
end
|
T0Mlet
(x0, tm1, tm2) =>
let
  val
  tp1 =
  tinfer1(tm1)
  val
  env =
  s0env_extend
  (env, x0, tp1)
in
  t0erm_tinfer1(tm2, env)
end
//
//
end // end of [let]

(* ****** ****** *)

(* end of [lambda2_typeck.dats] *)
