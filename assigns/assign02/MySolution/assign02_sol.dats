(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.sats"
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)
#staload "./lambda0_print.dats"
#staload "./lambda0_interp.dats"
#staload "./fcoin_change.dats"

#dynload "./lambda0_print.dats"
#dynload "./lambda0_interp.dats"
#dynload "./fcoin_change.dats"


(* ****** q1 variables ****** *)

val l0=mylist_nil{t0erm}()
val l1=mylist_cons{t0erm}(T0Mvar("x0"),l0)
val l2=mylist_cons{t0erm}(T0Mvar("x1"),l1)
val tup0=T0Mtup(l2)
val tpr0=T0Mprj(tup0,0)
val tup1=T0Mapp(T0Mlam("x0",tup0),T0Mvar("y"))

(* ***** q2 variables ****** *)

val num3=mylist_cons{t0erm}(T0Mint(25),mylist_nil())
val num2=mylist_cons{t0erm}(T0Mint(10),num3)
val num1=mylist_cons{t0erm}(T0Mint(5),num2)
val num0=mylist_cons{t0erm}(T0Mint(1),num1)
val tlist=T0Mtup(num0)
val tsum=T0Mvar("sum")
val tn=T0Mvar("n")
val aux=T0Mvar("aux")
val x=300//argument

(* Implementing coin_get in Lambda0 *)
val coin_get=
T0Mlam("tup",
T0Mlam("n",
	T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(0)),
		T0Mprj(T0Mvar("tup"),0)
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(1)),
		T0Mprj(T0Mvar("tup"),1)
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(2)),
		T0Mprj(T0Mvar("tup"),2)
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(3)),
		T0Mprj(T0Mvar("tup"),3)
	,
		T0Mint(0)
	)
	)
	)
	)
	)
)
(* end of implementing coin_change in Lambda0 *)




(* Implementing coin_change in Lambda0 *)
val aux_fun=
T0Mfix1("aux",
T0Mlam("sum",
T0Mlam("n",
T0Mcond(T0Mopr2(">",tsum,T0Mint(0)),
	T0Mcond(T0Mopr2(">=",tn,T0Mint(0)),
		T0Mopr2("+",
			T0Mapp(T0Mapp(aux,tsum),T0Mopr2("-",tn,T0Mint(1))),
			T0Mapp(T0Mapp(aux, T0Mopr2("-",tsum,T0Mapp(T0Mapp(coin_get,tlist),tn))),tn)
		    )
	,
		T0Mint(0)
	),	
		T0Mcond(T0Mopr2("<",tsum,T0Mint(0)),
			T0Mint(0)
		,
			T0Mint(1)

		)
)
)
)
)

val coin_change=T0Mlam("sum",T0Mapp(T0Mapp(aux_fun, T0Mvar("sum")),T0Mint(3)))
(* end of implementing coin_change in Lambda0 *)


implement
main0() =
{
val()=println!("*** Q1: Implementing T0Mtup and T0mPrj ***")
val()=println!("\nTesting print for T0Mtup:")
val()=println!("tup0 = ", tup0)

val()=println!("\nTesting print for T0Mprj:")
val()=println!("tpr0 = ", tpr0)

val()=println!("\nTesting interp(T0Mprj) without substitution:")
val()=println!("interp(", tpr0, ") = " ,t0erm_interp(tpr0))

val()=println!("\nTesting interp(T0Mtup) with substitution:")
val()=println!("interp(", tup1, ") = " ,t0erm_interp(tup1))

val()=println!("\nTesting interp(T0Mprj) with substitution:")
val()=println!("interp(", T0Mprj(tup1,1) , ") = " ,t0erm_interp(T0Mprj(tup1,1)))

val()=println!("\n\n*** Q2: Implementing coin_change by lambda0 ***")
val()=println!("\nThe coins = ",tlist)
val()=println!("\nTesting lambda based coin_get function :")
val()=println!("coin_get(list,1) = ", t0erm_interp(T0Mapp(T0Mapp(coin_get,tlist),T0Mint(1))))
val()=println!("\nTesting coin_change function: ")
val()=println!("The result from coin_change(",x,") which is impelemented by ATS = ", fcoin_change(x))
val()=println!("The result from coin_change(",x,") which is impelemented by lambda0 = ", t0erm_interp(T0Mapp(coin_change,T0Mint(x))))

}

(* end of [lambda0_main.dats] *)
