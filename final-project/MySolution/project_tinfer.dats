(* ****** ****** *)
#staload "./project.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)

extern fun t1dclist_tinfer(t1dclist):void
extern fun remove_links(type1):type1
implement remove_links(tp1a)=(
  case+ tp1a of
  | T1Pext(tex0) =>
  (
      case tex0.get() of
      | myoptn_cons(tex) =>
        remove_links(tex)
      | myoptn_nil() =>
        tp1a
  )    
  | _ =>
    tp1a
)
(* ****** ****** *)

local

fun
auxvar
( t1m0
: t1erm): type1 =
let
val-
T1Mvar(t1v) = t1m0
//val ()=println!("Here at TOP!!!\n\n\n\n\n",t1v.type1(),"\n\n\n\n\n")
in
 t1v.type1()
end

fun
auxfst
( t1m0
: t1erm): type1 =
(
let
  val-
  T1Mfst(tup1) = t1m0
  val
  t1p1 =
  t1erm_tinfer(tup1)
  //val ()=println!("here tup type is ",t1p1)
in
  case+ t1p1 of
  |T1Pext(tpx0) =>
  (
    let
      val-true =
      type1_unify
      (t1p1, type1_new_tup())
      //val ()=println!("t1p1 is ", t1p1,"\n\n\n") 
      val top=tpext_get(tpx0)
    in
      case- top of 
      | myoptn_cons(t1)=>
      (
        let 
          val-T1Ptup(tfst, _) = t1
        in
          tfst
        end
      )
    end
  )
  |T1Ptup(tfst, _) =>
  tfst
  | _ =>
  let val() = print("Should be a tuple!")
  in
  t1p1
  end
end
)
fun
auxsnd
( t1m0
: t1erm): type1 =
(
 let
  val-
  T1Msnd(tup1) = t1m0
  val
  t1p1 =
  t1erm_tinfer(tup1)
  //val ()=println!("here tup type is ",t1p1)
in
  case+ t1p1 of
  |T1Pext(tpx0) =>
  (
    let
      val-true =
      type1_unify
      (t1p1, type1_new_tup())
      //val ()=println!("t1p1 is ", t1p1,"\n\n\n") 
      val top=tpext_get(tpx0)
    in
      case- top of 
      | myoptn_cons(t1)=>
      (
        let 
          val-T1Ptup(_, tsnd) = t1
        in
          tsnd
        end
      )
    end
  )
  |T1Ptup(_, tsnd) =>
  tsnd
  | _ =>
  let val() = print("Should be a tuple!")
  in
  t1p1
  end
end
)
in(*in-of-local*)

implement
t1erm_tinfer
  (t1m0) =
(
case+ t1m0 of
|
T1Mnil _ => T1Pnil
|
T1Mbtf _ => T1Pbool
|
T1Mint _ => T1Pint
|
T1Mstr _ => T1Pstring
|
T1Mvar _ => auxvar(t1m0)
//
| T1Mlam(x0 , tpr, tm) =>
  let
    val tptm=t1erm_tinfer(tm)
    //val ()=println!("tpr ",tpr)
    //val ()=println!("tptm ",tptm)
    val-true=type1_unify(tptm,tpr)
    val tpr2=remove_links(tpr)
    val tpa=remove_links(x0.type1())
  in
    T1Pfun(tpa,tpr2)
  end
| T1Mfix(x0 , tp, tm) =>
  let
    val tptm=t1erm_tinfer(tm)
    //val ()=println!("tpr ",tpr)
    //val ()=println!("tptm ",tptm)
    val-true=type1_unify(tptm,tp)
  in
    remove_links(tptm)
  end
|
T1Mapp(tm1, tm2) =>
let
  val tp1 = t1erm_tinfer(tm1)
  val tp2 = t1erm_tinfer(tm2)
in
  case+ tp1 of
  | T1Pext(tex) =>
    let
      //val ()=println!(".....here.......",tp2,"............")
      val targ=T1Pext(tpext_new())
      val tres=T1Pext(tpext_new())
      //val ()=println!("term=\n",t1m0,"\n","type of tm1=\n",tp1,"\ntype of tm2=\n",tp2,"\n")
      //val-true = type1_unify(targ,tp2)
      val-true = type1_unify(T1Pfun(targ,tres),tp1)
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
//
|T1Mlet(tdl, tm) =>
  let
    val ()=t1dclist_tinfer(tdl)
  in
    t1erm_tinfer(tm)
  end
|
T1Mfst _ => auxfst(t1m0)
|
T1Msnd _ => auxsnd(t1m0)
//
|
T1Mtup(t1m1, t1m2) =>
(
  T1Ptup(t1p1, t1p2)
) where
{
  val
  t1p1 = t1erm_tinfer(t1m1)
  val
  t1p2 = t1erm_tinfer(t1m2)
}
//
| T1Mopr1(opr, tm1) =>
let
val tp1 = remove_links(t1erm_tinfer(tm1))
in
  case+ opr of
  | "print" =>
  (
  case- tp1 of
  | T1Pbas(nm1)
    when nm1 = "nil" => T1Pnil
  | T1Pbas(nm1)
    when nm1 = "int" => T1Pnil
  | T1Pbas(nm1)
    when nm1 = "bool" => T1Pnil
  | T1Pbas(nm1)
    when nm1 = "string" => T1Pnil
  )
  | _ =>
    tp1
end
| T1Mopr2(opr, tm1,tm2) =>
  let 
    val tp1=t1erm_tinfer(tm1)
    val tp2=t1erm_tinfer(tm2)
    val-true = type1_unify(tp1,T1Pint)
    val-true = type1_unify(tp2,T1Pint)
  in
    case+ opr of
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
      | _ (* else *) =>
        tp1
  end
//
|
T1Manno(tm1, tp2) =>
let
  val tp1 = t1erm_tinfer(tm1)
  val-true = type1_unify(tp1, tp2)
in
  tp2
end
|
T1Mcond(tmc, tm1,tm2) =>
let
  val tpc=t1erm_tinfer(tmc)
  val tp1=t1erm_tinfer(tm1)
  val tp2=t1erm_tinfer(tm2)
  val-true = type1_unify(tpc, T1Pbool)
  val-true = type1_unify(tp1, tp2)
in
  tp1
end
)
//where
//{

//val () =
//println!
//("t1erm_tinfer: t1m0 = ", t1m0)

//} (* end of [t0erm_tinfer] *)

end // end of [local]
implement t1dcl_tinfer(td)=
(
  let
    val T1DCL(tv,tm)=td
    val tvtp=t1var_get_type1(tv)
    val tp=t1erm_tinfer(tm)
    val-true=type1_unify(tvtp,tp)
  in
    ()
  end
  
)

implement t1dclist_tinfer(tdl):void=
(
  case+ tdl of
  | mylist_cons(td,tail) =>
    let 
    val ()=t1dcl_tinfer(td)
    in
      t1dclist_tinfer(tail)
    end
  | mylist_nil() =>
  ()
)
implement t1pgm_tinfer(tg)=
(
  let
    val T1PGM(tdl,tm)=tg
    val ()=t1dclist_tinfer(tdl)
  in
    t1erm_tinfer(tm)
  end
  
)
(* ****** ****** *)

(* end of [project_tinfer.dats] *)