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
fprint_val<t1erm> = fprint_t1erm
implement
fprint_val<t1dcl> = fprint_t1dcl
implement
fprint_val<t1pgm> = fprint_t1pgm
implement
fprint_val<type1> =  fprint_type1

implement
print_t1erm(tm) =
fprint_t1erm
(stdout_ref, tm)
implement
print_t1dcl(tdcl) =
fprint_t1dcl(stdout_ref, tdcl)
implement
print_t1pgm(tpgm) =
fprint_t1pgm(stdout_ref, tpgm)

(* ****** ****** *)

(* ****** ****** *)
implement
fprint_t1dcl
(out, tdcl) =
(
case+ tdcl of
| T1DCL(x0, def) =>
  fprint!
  (out, "T1DCL(", x0, ", ", def, ")")
)

implement
fprint_t1pgm
(out, tpgm) =
(
case+ tpgm of
| T1PGM(tdcl, tm) =>
  fprint!
  (out, "T1PGM(", tdcl, ", ", tm, ")")
)
(* ****** ****** *)

local

(* ****** ****** *)

absimpl
t1erm_tbox = $rec
{ t1erm_type= type1
, t1erm_node= t1erm_node
}

(* ****** ****** *)

in(* in-of-local *)

(* ****** ****** *)

implement
t1erm_get_node
  (t1m) = t1m.t1erm_node
implement
t1erm_get_type
  (t1m) = t1m.t1erm_type

(* ****** ****** *)

implement
t1erm_make2
(type, node) =
(
$rec
{ t1erm_type= type
, t1erm_node= node
}
) (* end of [t1erm_make2] *)

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

implement
t1erm_make1
(node) =
t1erm_make2
(type, node) where
{
val type = type1_new_ext()
} (* end of [t1erm_make1] *)

(* ****** ****** *)

implement
t1erm_nil
  () =
t1erm_make2
(T1Pnil, T1Mnil())

implement
t1erm_int
  (i0) =
t1erm_make2
(T1Pint, T1Mint(i0))

implement
t1erm_btf
  (b0) =
t1erm_make2
(T1Pbool, T1Mbtf(b0))

implement
t1erm_str
  (s0) =
t1erm_make2
(T1Pstring, T1Mstr(s0))

(* ****** ****** *)

implement
t1erm_var
  (t1v) =
t1erm_make2
( t1p
, T1Mvar(t1v)) where
{
  val t1p = t1v.type()
}

(* ****** ****** *)

implement
t1erm_tup
  (tm1,tm2) =
t1erm_make2
( T1Ptup(tp1, tp2)
, T1Mtup(tm1,tm2)) where
{
  val tp1 = tm1.type()
  val tp2 = tm2.type()
  val ()=println!("########## type of tuple is: tup(",tp1,",",tp2,")")
}


(* ****** ****** *)

implement
t1erm_fst
  (tm) =
    t1erm_make2(tp, T1Mfst(tm)) where
  {
    val tp=
    (
    let
      val tptup=tm.type()
      val ()=println!("**********************fst of :    ",tptup)
    in
    case- tptup of
    | T1Ptup(tp1,tp2) =>
      tp1
    | T1Pext(tex) =>
    (
      let 
        val newtup=type1_new_tup()
        val-T1Ptup(tp1,tp2)=newtup
        val ()=tex.set(newtup)
        
      in
        tp1
      end
    )
    end
    ):type1
    val()= println!("term= T1Mfst(",tm,") --- type= ",tp)
  }

(* ****** ****** *)

implement
t1erm_snd
  (tm) =
    t1erm_make2(tp, T1Msnd(tm)) where
  {
    val tp=
    (
    let
      val tptup=tm.type()
      val ()=println!("**********************snd of :    ",tptup)
    in
    case- tptup of
    | T1Ptup(tp1,tp2) =>
      tp2
    | T1Pext(tex) =>
    (
      let 
        val newtup=type1_new_tup()
        val-T1Ptup(tp1,tp2)=newtup
        val ()=tex.set(newtup)
        
        
      in
        tp2
      end
    )
    end
    ):type1

    val ()= println!("term= T1Msnd(",tm,") --- type= ",tp)
  }

(* ****** ****** *)

implement
t1erm_lam
  (t1v,tpr,tm) =
t1erm_make2
( T1Pfun(t1p,tpr)
, T1Mlam(t1v,t1p,tm)) where
{
  val t1p = t1v.type()
  val ()=println!("+++++++++++++++++ type of lam is: ",T1Pfun(t1p,tpr))
}

(* ****** ****** *)

implement
t1erm_fix
  (t1v, tp1, tm) =
t1erm_make2
( tp1
, T1Mfix(t1v,tp1,tm)) where
{
  val t1p = t1v.type()
}


(* ****** ****** *)

fun app_tinfer(tm1:t1erm,tm2:t1erm):type1=
(
let
  val tp1 = tm1.type()
  val tp2 = tm2.type()
in
  case+ tp1 of
  | T1Pext(tex) =>
    let
      //val ()=println!(".....here.......",tp2,"............")
      val targ=T1Pext(tpext_new())
      val tres=T1Pext(tpext_new())
      //val ()=println!("term=\n",t1m0,"\n","type of tm1=\n",tp1,"\ntype of tm2=\n",tp2,"\n")
      //val-true = type1_unify(targ,tp2)
      val ()=println!("function type in app is: ",tp1)
      val ()=tex.set(T1Pfun(targ,tres))
    in
      tres
    end
  | _ =>
    let
      val ()=println!("..here...tp1.......",tp1,"............tp2.....",tp2)
      val-T1Pfun(targ, tres)=tp1
      val-true = type1_unify(targ, tp2)
    in
      tres
    end
end
)

implement
t1erm_app
(t1f1, t1a2) =
t1erm_make2(tap,T1Mapp(t1f1, t1a2)) where {
val tap=app_tinfer(t1f1,t1a2)

}


(* ****** ****** *)

implement
t1erm_let
  (tdl, tm) =
t1erm_make2
( t1p
, T1Mlet(tdl,tm)) where
{
  val t1p = tm.type()
}


(* ****** ****** *)


implement
t1erm_opr1
  (opr,tm1) =
t1erm_make2
( tp
, T1Mopr1(opr,tm1)) where
{
  val ()=println!("type of print: ",tm1.type())
  val tp=(
  case+ opr of
    | "print" =>
    (
    T1Pnil
    )
    | _ =>
      tm1.type()
    ):type1
}



(* ****** ****** *)

implement
t1erm_opr2
  (opr,tm1,tm2) =
t1erm_make2
( tp
, T1Mopr2(opr,tm1,tm2)) where
{
  val tp=(
  case- opr of
  | ">" =>
    T1Pbool
  | "<" =>
    T1Pbool
  | "=" =>
    T1Pbool
  | "<=" =>
    T1Pbool
  | ">=" =>
    T1Pbool
  | "==" =>
    T1Pbool
  | "==" =>
    T1Pbool
  | "!=" =>
    T1Pbool
  | _ =>
    tm1.type()
    ):type1
}

(* ***** ****** *)

implement t1erm_anno(tm, tp1)=
t1erm_make2
( tp1
, T1Manno(tm,tptm1)) where
{
  val tptm1 = tm.type()
  val-true=type1_unify(tp1,tptm1)
}

(* ***** ****** *)

implement t1erm_cond(tmc, tmi, tme)=
t1erm_make2
( tp1
, T1Mcond(tmc,tmi,tme)) where
{
  val tp1 = tmi.type()
}


(* ***** ****** *)
implement
fprint_t1erm
(out, tm) =
(
case+ tm.node() of
| T1Mnil() =>
  fprint!(out, "T1Mnil(", ")")
//
| T1Mbtf(x0) =>
  fprint!(out, "T1Mbtf(", x0, ")")
//
| T1Mint(x0) =>
  fprint!(out, "T1Mint(", x0, ")")
//
| T1Mstr(x0) =>
  fprint!(out, "T1Mstr(", x0, ")")
//
| T1Mvar(x0) =>
  fprint!(out, "T1Mvar(", x0, ")")
//
| T1Mlam(x0, tp1, tm2) =>
  fprint!
  ( out
  , "T1Mlam(", x0, "; ", tp1, "; ", tm2,")")
| T1Mapp(tm1, tm2) =>
  fprint!(out, "T1Mapp(", tm1, "; ", tm2, ")")
//
| T1Mlet(tdl, tm1) =>
  fprint!
  ( out
  , "T1Mlet(",tdl,"; ", tm1, ")")
//
| T1Mfix(f0,tp1, tm1) =>
  fprint!
  ( out
  , "T1Mfix(", f0, "; ", tp1, "; ",tm1, ")")
//
| T1Mfst(tup) =>
  fprint!(out, "T1Mfst(", tup, ")")
//
| T1Msnd(tup) =>
  fprint!(out, "T1Msnd(", tup, ")")
//
| T1Mtup(tm1, tm2) =>
  fprint!(out, "T1Mtup(", tm1, "; ", tm2, ")")
//
| T1Mopr1(opr, tm1) =>
  fprint!(out, "T1Mopr1(", opr, "; ", tm1, ")")
//
| T1Mopr2(opr, tm1, tm2) =>
  fprint!
  ( out
  , "T1Mopr2(", opr, "; ", tm1, "; ", tm2, ")")
//
| T1Manno(tm1, tp2) =>
  fprint!(out, "T1Manno(", tm1, "; ", tp2, ")")
//
| T1Mcond(tm1, tm2, tm3) =>
  fprint!
  ( out
  , "T1Mcond(", tm1, "; ", tm2, "; ", tm3, ")")
//
)

(* end of [project_t1erm.dats] *)
