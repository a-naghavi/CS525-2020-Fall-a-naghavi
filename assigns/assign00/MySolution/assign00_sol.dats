#include "share/atspre_staload.hats"


//***********************************************************
fun int_test():int = 
	let fun calculate (x:int,n:int):int=
		if x>0 then calculate(x+x,n+1) else n
	in 
		calculate(1,1)
	end
//************************************************************
#define N 5
fun ghaap(n: int): int =
(
  if
  (n >= 2)
  then n * ghaap(n-1) * ghaap(n-2)
  else (n+1)
  // end of [if]
)

fun gheep(n:int): int =
	let fun tgheep(n:int,i:int,r1:int,r2:int):int=
	(
		if (n>=i) then tgheep(n,i+1,i*r1*r2,r1) else r1
		
	)
	in
		tgheep(n,2,2,1)
	end
//************************************************************

//Amin: I have implemented this function
//in both recursive and tail-recursive
//I have commented the recursive implemetion.

datatype
intlist =
| intlist_nil of ()
| intlist_cons of (int, intlist)

#define nil intlist_nil
#define :: intlist_cons
#define cons intlist_cons

extern fun intlist_append : (intlist, intlist) -> intlist

//recursive function
(*)
fun rec_intlist_append(l1:intlist,l2:intlist):intlist =
(
	case l1 of
	| intlist_nil() => l2
	| intlist_cons(hd,tl) => intlist_cons(hd,rec_intlist_append(tl,l2))
)
*)
//tail recursive function

implement intlist_append(l1,l2) =

	let fun toend(r:intlist,l1:intlist,l2:intlist):intlist =
	(
		let fun tappend(r:intlist,l:intlist):intlist =
		(
			case l of
			| intlist_nil() => r
			| intlist_cons(hd,tl) => tappend(intlist_cons(hd,r),tl)
		)
		in
			case l1 of
			| intlist_nil() => tappend(l2,r)
			| intlist_cons(hd,tl) => toend(intlist_cons(hd,r),tl,l2)
		end

	)	
	in
		toend(intlist_nil,l1,l2)
	end

//printing the list on screen
fun printlist(l:intlist): void =
	let fun printl(l:intlist): void =
	(
		case l of 
		| intlist_cons(hd,tl) =>
		(
			print!(hd,", ");
			printl(tl);
		)
		| intlist_nil() =>
			print ("]\n")
	)
	in
	(
		print("[");
		printl(l);
	)
	end

//**************************************************************
val l1= 1::2::3::nil
val l2 = 4::5::6::nil
val l3 = intlist_append(l1,l2)
//val l4 = rec_intlist_append(l1,l2)
implement main0()=
(
println!("The size of an integer in C is: ", int_test());
println!("ghaap(",N,"):", ghaap(N)," = gheep(",N,"):", gheep(N));
print("list1 = ");
printlist(l1);
print("list2 = ");
printlist(l2);
print("concat(list1,list2) = ");
printlist(l3);
//print("recursive_concat(list1,list2) = ");
//printlist(l4);
)