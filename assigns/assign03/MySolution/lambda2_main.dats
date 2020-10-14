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

#dynload "./lambda2_type.dats"
#dynload "./lambda2_term.dats"
#dynload "./lambda2_senv.dats"
#dynload "./lambda2_tcheck.dats"


(* ****** ****** *)

(* ****** q1 variables for test ****** *)
val topr11=T0Mopr1("~",T0Mbool(true))
val topr12=T0Mopr1("-",T0Mint(2))
val topr21=T0Mopr2(">",T0Mint(1),T0Mint(2))
val topr22=T0Mopr2("+",T0Mint(1),T0Mint(2))
val ttup=T0Mtup(T0Mint(1),T0Mbool(true))
val tfst=T0Mfst(ttup)
val tsnd=T0Msnd(ttup)
val tcond=T0Mcond(topr21,T0Mint(0),T0Mint(1))
val tfix=T0Mfix1("f",T0Pfun(T0Pint,T0Pint),
	T0Mlam("x",T0Pint, T0Mint(1)))

(* ****** q2 variables ****** *)

val num2=T0Mtup(T0Mint(10),T0Mint(25))
val num1=T0Mtup(T0Mint(5),num2)
val tlist=T0Mtup(T0Mint(1),num1)
val tsum=T0Mvar("sum")
val tn=T0Mvar("n")
val aux=T0Mvar("aux")
val x=300//argument

(* Implementing coin_get in Lambda2 *)
val coin_get=
T0Mlam("tup",T0Ptup(T0Pint,T0Ptup(T0Pint,T0Ptup(T0Pint,T0Pint))),
T0Mlam("n",T0Pint,
	T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(0)),
		T0Mfst(T0Mvar("tup"))
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(1)),
		T0Mfst(T0Msnd(T0Mvar("tup")))
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(2)),
		T0Mfst(T0Msnd(T0Msnd(T0Mvar("tup"))))
	, T0Mcond(T0Mopr2("=",T0Mvar("n"),T0Mint(3)),
		T0Msnd(T0Msnd(T0Msnd(T0Mvar("tup"))))
	,
		T0Mint(0)
	)
	)
	)
	)
	)
)
(* end of implementing coin_change in Lambda2 *)




(* Implementing coin_change in Lambda2 *)
val aux_fun=
T0Mfix1("aux",T0Pfun(T0Pint,T0Pfun(T0Pint,T0Pint)),
T0Mlam("sum",T0Pint,
T0Mlam("n",T0Pint,
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

val coin_change=T0Mlam("sum",T0Pint,T0Mapp(T0Mapp(aux_fun, T0Mvar("sum")),T0Mint(3)))
(* end of implementing coin_change in Lambda2 *)


(* ****** ****** *)

implement main0() = {
val ()=println!("****************** Q1 ********************\n")
val ()=println!("*Type of ", topr11 ,"= ",t0erm_tcheck0(topr11))
val ()=println!("*Type of ", topr12 ,"= ",t0erm_tcheck0(topr12))
val ()=println!("*Type of ", topr21 ,"= ",t0erm_tcheck0(topr21))
val ()=println!("*Type of ", topr22 ,"= ",t0erm_tcheck0(topr22))
val ()=println!("*Type of ", ttup ,"= ",t0erm_tcheck0(ttup))
val ()=println!("*Type of ", tfst ,"= ",t0erm_tcheck0(tfst))
val ()=println!("*Type of ", tsnd ,"= ",t0erm_tcheck0(tsnd))
val ()=println!("*Type of ", tcond ,"= ",t0erm_tcheck0(tcond))
val ()=println!("*Type of ", tfix ,"= ",t0erm_tcheck0(tfix))

val ()=println!("\n****************** Q2 ********************\n")
val ()=println!("*Type of coinlist input=\n",t0erm_tcheck0(tlist))
val ()=println!("\n*Type of coin_get function=\n",t0erm_tcheck0(coin_get))
val ()=println!("\n*Type of return value of coin_get(list,int) function=\n",t0erm_tcheck0(T0Mapp(T0Mapp(coin_get,tlist),T0Mint(1))))
val ()=println!("\n*Type of aux function=\n",t0erm_tcheck0(aux_fun))
val ()=println!("\n*Type of coin_change function=\n",t0erm_tcheck0(coin_change))
val ()=println!("\n*Type of return value for coin_change(int) function=\n",t0erm_tcheck0(T0Mapp(coin_change,T0Mint(x))))

}


(* ****** ****** *)

(* end of [lambda2_main.dats] *)
