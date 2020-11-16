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

extern
fun
trans01_type: type0 -> type1
(* ****** ****** *)

extern
fun
trans01_term(s0env, t0erm): t1erm
extern
fun
trans01_tdcl(s0env, t0dcl): t1dcl


(* ****** ****** *)
(* ******* A ******* *)
extern fun remove_from_env(tm1:t0erm,env: s0env):s0env

implement remove_from_env(tm0,env)=
(
  case- tm0 of 
  |
  T0Mvar(x0) =>
    s0env_remove(env,x0)
  | _ => env
)

implement trans01_tdcl(env,td0) =
(
	let
		val T0DCL(x,t0)=td0
	in
		T1DCL(t1var_new(x),trans01_term(env,t0))
	end
)

fun trans01_tdclist(env:s0env,tdl0:t0dclist):t1dclist =
(
	case tdl0 of 
	| mylist_cons(td0,tail) =>
		mylist_cons(trans01_tdcl(env,td0),trans01_tdclist(env,tail))
	|	mylist_nil() =>
		mylist_nil()
)

fun get_tdclist_env(env:s0env,tdl1:t1dclist):s0env=
(
	case tdl1 of 
	| mylist_cons(td1,tail) =>
	let 
		val T1DCL(tv1,tm1)=td1
		val tv0=t1var_get_name(tv1)
		val tp1=t1var_get_type1(tv1)
	in
		get_tdclist_env(s0env_extend(env,tv0,tp1),tail)
	end
	| mylist_nil() =>
		env
)

fun auxtup(env:s0env,tlm0:mylist(t0erm)):t1erm=
(
	let 
	fun auxtup1(tl0:mylist(t0erm))=
	(
		case tl0 of
		| mylist_cons(hd0,tail0) =>
		(
			case tail0 of
			| mylist_cons(hd1,tail1)  =>
				T1Mtup(trans01_term(env,hd0),auxtup1(tail0))
			| mylist_nil() =>
				T1Mtup(trans01_term(env,hd0),T1Mnil)
		) 
		| mylist_nil() =>
			T1Mtup(T1Mnil,T1Mnil)
	)
	in
		auxtup1(tlm0)
	end
)

fun auxprj(env:s0env,t0:t0erm,i:int):t1erm =
(
let
	val-t1t=trans01_term(env,t0)
	fun prjcounter(t1:t1erm,i:int):t1erm =
	(
	if i> 0 then
		prjcounter(T1Msnd(t1),i-1)
	else
		T1Mfst(t1)
	)
in
	prjcounter(t1t,i)
end
)

fun auxoprs(env:s0env,t0:t0erm):t1erm=		
(
let 
val-T0Moprs(o,tl0)=t0
fun oprs(tl0:mylist(t0erm)):t1erm =
	(
	case tl0 of
	| mylist_cons(hd0,tail0) =>
	(
		case tail0 of
		| mylist_cons _ =>
			T1Mopr2(o,trans01_term(env,hd0),oprs(tail0))
		| mylist_nil() =>
			trans01_term(env,hd0)
	)
	| mylist_nil() =>
		T1Mopr1(o,T1Mnil)
	)
in
	case tl0 of
	| mylist_cons(hd0,tail0) =>
	(
		case tail0 of
		| mylist_cons _ =>
			oprs(tl0)
		| mylist_nil() =>
			T1Mopr1(o,trans01_term(env,hd0))
	)
	| mylist_nil() =>
		T1Mopr1(o,T1Mnil)
end
)


(* ******* A ******* *)
implement
trans01_term
  (env0, tm0) =
let
//
fun
trans01
(tm: t0erm): t1erm =
trans01_term(env0, tm)
//
in
//
case- tm0 of
	| T0Mnil() =>
		T1Mnil()
	//
	| T0Mbtf(v) =>
		T1Mbtf(v)

	| T0Mint(v) =>
		T1Mint(v)

	| T0Mflt(v) =>
		exit(1)

	| T0Mstr(v) =>
		T1Mstr(v)

	| T0Mvar(x) =>
	let
		val
		opt = s0env_search(env0, x)
	//
	in
		case opt of 
		| myoptn_cons(tp1) =>
			T1Mvar(t1var_make(x,tp1))
		| myoptn_nil() =>
			T1Mvar(t1var_new(x))


	end // end of [T0Mvar]

	| T0Mlam(x,tp0a,tr,tp0r) =>
	(
		let
		  val
		  tp1 =
		  (
		  case+ tp0a of
		  | myoptn_nil() =>
		    type1_new_ext()
		  | myoptn_cons(tp0) => trans01_type(tp0)
		  ) : type1 // end-of-val
		  val
		  new_env = s0env_extend(env0, x, tp1)
  		  val
		  tp2 =
		  (
		  case+ tp0r of
		  | myoptn_nil() =>
		    type1_new_ext()
		  | myoptn_cons(tp0) => trans01_type(tp0)
		  ) : type1 // end-of-val
		  val
		  new_env = s0env_extend(env0, x, tp1)
		in
			T1Mlam(t1var_make(x,tp1),tp2,trans01_term(new_env,tr))
		end
	)
	| T0Mfix(x,tr) =>
		T1Mfix(t1var_new(x),T1Pext(tpext_new()),trans01(tr))

	| T0Mapp(t0, t1) =>
	let
	  val t0t1 = trans01(t0)
	  val t1t1 = trans01(t1)
	in
		T1Mapp(t0t1,t1t1)
	end
	| T0Mlet(tdl0, t0 ) =>
		let 
			val tdl1=trans01_tdclist(env0,tdl0)
			val new_env=get_tdclist_env(env0,tdl1)
		in
			T1Mlet(tdl1,trans01_term(new_env,t0))
		end
	| T0Mopr1(o, t0) =>
		T1Mopr1(o, trans01(t0))

	| T0Mopr2(o, t0, t1) =>
		T1Mopr2(o, trans01(t0), trans01(t1))

	| T0Moprs(o, tl0) =>
		auxoprs(env0,tm0)

	| T0Mtup(tl0) =>
	(
		case tl0 of
		|mylist_cons(hd0,tail0) =>
		(
			case tail0 of
			| mylist_cons(hd1,tail1) =>
				auxtup(env0,tl0)
			| mylist_nil() =>
				trans01(hd0)
		)
		|mylist_nil() =>
			T1Mnil
	)
	//
	| T0Mprj(t0, i) =>
		auxprj(env0,t0,i)
	//
	| T0Manno(t0, tp0) =>
		T1Manno(trans01(t0),trans01_type(tp0))

	| T0Mcond(tc, t0, to1) =>
		(
		case to1 of
		| 
		myoptn_nil() => T1Mcond(trans01(tc),trans01(t0),T1Mnil())
		| 
		myoptn_cons(t1) => T1Mcond(trans01(tc),trans01(t0),trans01(t1))
		)
//
//|
//T0Mvar(t0v) =>
//let
//val
//opt =
//s0env_search(env0, t0v)
//in
//case- opt of
//| myoptn_cons(t1m) => t1m
//end
//
end // end of [trans01_term]

(* ****** ****** *)



implement trans01_tpgm(pg0: t0pgm) =
(
	let 
		val T0PGM( tdl0, t0) = pg0	
		val env0=s0env_nil()
		val tdl1=trans01_tdclist(env0,tdl0)
		val new_env=get_tdclist_env(env0,tdl1)
	in
		T1PGM(tdl1,trans01_term(new_env,t0))
	end
)
(* ****** ****** *)

(* ****** ****** *)
implement
trans01_type (tp00) =
(
	case+ tp00 of
	| T0Pbas(b) =>
		T1Pbas(b)
	| T0Pfun(t0, t1) =>
		T1Pfun(trans01_type(t0),trans01_type(t1))
	| T0Ptup(t0, t1) =>
		T1Ptup(trans01_type(t0),trans01_type(t1))
)


(* ****** ****** *)
(* end of [project_trans01.dats] *)
