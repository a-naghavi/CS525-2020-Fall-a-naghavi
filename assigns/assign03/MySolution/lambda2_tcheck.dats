(* ****** ****** *)
#staload "./lambda2.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.sats"
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)

val
senv_nil =
SENV(mylist_nil())

(* ****** ****** *)

implement
t0erm_tcheck0(prog) =
t0erm_tcheck1(prog, senv_nil)

(* ****** ****** *)

implement
t0erm_tcheck1
  (tm0, senv) =
let
//
fun
tcheck1
(tm: t0erm): type0 =
t0erm_tcheck1(tm, senv)
//
in
case- tm0 of
|
T0Mint(_) => T0Pint
|
T0Mbool (_) => T0Pbool
//
|
T0Mvar(x0) =>
let
val
opt = senv_search(senv, x0)
//
in
case- opt of myoptn_cons(tp) => tp
end // end of [T0Mvar]
//
|
T0Mtup(tm1, tm2) =>
(
T0Ptup(tp1, tp2)
) where
{
  val tp1 = tcheck1(tm1)
  val tp2 = tcheck1(tm2)
}
//
|
T0Mlam
(x0, tp1, tm2) =>
let
  val
  senv =
  senv_extend
  (senv, x0, tp1)
  val tp2 =
  t0erm_tcheck1(tm2, senv)
in
  T0Pfun(tp1, tp2)
end
//
|
T0Mapp(tm1, tm2) =>
  let
    val tp1 = tcheck1(tm1)
    val tp2 = tcheck1(tm2)
  in
    case tm1 of
    | T0Mfix1 _ =>
      let
       //val()=println!("here I am in fix",tp1)
        val-T0Pfun(targ,tres)=tp1
        val-true = eq_type0_type0(targ, tp2)
      in
        tres
      end
    | T0Mvar(x0) =>
      let
       //val()=println!("here I am in var",tp1)
        
        val-T0Pfun(targ,tres)=tp1
        val-true = eq_type0_type0(targ, tp2)
      in
        tres
      end
    | _ =>
      let
        //val()=println!("here I am in fun",tp1)
        val-T0Pfun(targ, tres)=tp1
        val-true = eq_type0_type0(targ, tp2)
      in
        tres
      end
      // tm1 should be a function!
  end
//
|
T0Manno(tm1, tp2) =>
let
  val tp1 = tcheck1(tm1)
(*
  val ( ) = println!("tp1 = ", tp1)
  val ( ) = println!("tp2 = ", tp2)
*)
  val-true = eq_type0_type0(tp1, tp2)
in
  tp2
end
//
|
T0Mopr1(opr, t1) => 
let 
  val tp1=tcheck1(t1)
in
  case+ opr of
  | "-" => 
    let 
      val T0Pint = tp1
    in
      T0Pint
    end 
  | "~" => 
    let 
      val T0Pbool = tp1
    in
      T0Pbool
    end 
  | _ =>
    tp1
end
|
T0Mopr2(opr, t1, t2) =>
let
  val tp1=tcheck1(t1)
  val tp2=tcheck1(t2)

in
  case+ opr of
  | "+" => 
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pint
  end
  | "-" => 
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pint
  end
  | "*" => 
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pint
  end
//
  | ">" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
  | "<" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
  | "<=" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
  | ">=" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
  | "=" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
  | "!=" =>
  let
    val T0Pint = tp1
    val T0Pint = tp2
  in
    T0Pbool
  end
//
  | _ =>
  let
    val-true=eq_type0_type0(tp1,tp2)
  in
    tp1
  end
end
|
T0Mfix1(x0, tp1, t1) =>
let
  val senv =
    senv_extend
    (senv, x0, tp1)
  val tpbody=t0erm_tcheck1(t1, senv)
  val-true=eq_type0_type0(tpbody,tp1)
  //you must check with environment

in
  tp1
end
|
T0Mcond(cond, t1, t2) =>
let
  val tpc=tcheck1(cond)
  val T0Pbool = tpc
  val tp1 = tcheck1(t1)
  val tp2 = tcheck1(t2)
  val-true =eq_type0_type0(tp1, tp2)
in
  tp1
end
| T0Mfst(t1) =>
let
  val-T0Ptup(tfp,tsp) = tcheck1(t1)
in
  tfp
end
| T0Msnd(t1) =>
let
  val-T0Ptup(tfp,tsp) = tcheck1(t1)
in
  tsp
end
end // end of [let]

(* ****** ****** *)
////

implement
t0erm_typeck0
  (env?. prog) =
(
case+ prog of
| T0Mint(_) => T0Pint
//
| T0Mvar(x0) =>
//
//
)

(* ****** ****** *)

(* end of [lambda2_typeck.dats] *)
