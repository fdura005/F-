module Interp

// Skeleton file for PCF interpreter

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

// e is the body, x is the term to subsitute, t is the subsitution term
let rec subst e x t= match e with
                     |ID y              -> if x = y then t else ID y
                     |APP (e1, e2)      -> APP ( subst e1 x t, subst e2 x t)
                     |IF (e1, e2, e3)   -> IF (subst e1 x t, subst e2 x t, subst e3 x t)
                     |FUN (y, e1)       -> if x = y then FUN (y, e1) else FUN (y, subst e1 x t)
                     |REC (y, e1)       -> if x = y then REC (y, e1) else  REC (y, subst e1 x t)
                     |e -> e // Catch all


// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    |(ERROR s, _)        -> ERROR s        // ERRORs are propagated
    |(_, ERROR s)        -> ERROR s
    |(SUCC, NUM n)       -> NUM (n+1)      // Rule (6)
    |(SUCC, v)           -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    |(PRED, NUM 0)       -> NUM 0
    |(PRED,NUM n)        -> NUM (n-1)
    |(PRED, v)           -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    |(ISZERO, NUM n)     -> if (n=0) then BOOL true else BOOL false
    |(ISZERO,v)          -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
    |(FUN (x, e), v1)    -> interp (subst e x v1)
    |(REC (x, f), e)     -> interp (APP (subst f x (REC (x, f)), e))

    | x -> failwith (sprintf"Error!... %A " x)

|IF(e1,e2,e3)->
    match (interp e1,e2,e3) with
    |(ERROR s,_,_)  -> ERROR s 
    |(_,ERROR s,_)  -> ERROR s 
    |(_,_,ERROR s)  -> ERROR s 
    | (BOOL true, e, _)     -> interp e
    | (BOOL false, _, e) -> interp e
    |(_)->ERROR "Error!!!....'if' needs BOOL expression!..."

| e -> e  // Catch all case NUM , BOOL , FUN (x,e) -> FUN (x,e)  etc..

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp

let testInterp()=
    //testing some expressions
    let ans   = interpstr"succ 5"//6
    let ans1  = interpstr"pred 5"//4
    let ans3  = interpstr"if iszero 5 then pred (pred 5) 
                                      else succ (succ 6) " //8
    
    let ans4  = subst (NUM 6) "a" (NUM 3) //This is ans4: NUM 6
    let ans5  = subst (IF (BOOL true, FUN ("a", APP (SUCC, ID "a")), FUN ("b", APP (SUCC, ID "a")))) "a" (NUM 3)
    let ans6  = subst SUCC "a" (NUM 3) 
    
    ///////////////////////// Double ///////////////////////////////////////////////////////////////////////
    let ans7  = interpstr"(rec d -> fun n -> if iszero n then 0 else succ (succ (d (pred n)))) 37"
                //This is ans7: NUM 74
    
    ///////////////////////// fibonacci ///////////////////////////////////////////////////////////////////
    let ans8  = interpstr"let minus = rec m -> fun x -> fun y -> if iszero y then x else m (pred x) (pred y)
                          in
                            minus 125 79" //This is ans8: NUM 46

    let ans9  = interpstr"let plus = rec p ->
                                fun x -> fun y -> if iszero x then y else p (pred x) (succ y)
                          in
                            let fibonacci = rec f ->
	                                fun n -> if iszero n then
		                                        0
		                                     else if iszero (pred n) then
		                                        1
		                                     else
		                                        plus (f (pred n)) (f (pred (pred n)))
                            in
                              fibonacci 20" //This is ans9: NUM 6765

    ////////////////////////// factorial ///////////////////////////////////////////////////////////////////////
    let ans10  = interpstr"let plus = rec p ->
                                fun x -> fun y -> if iszero x then y else p (pred x) (succ y)
                           in
                              let times = rec t ->
	                                fun x -> fun y -> if iszero x then 0 else plus y (t (pred x) y)
                              in
                                let factorial = rec f ->
	                                    fun n -> if iszero n then 1 else times n (f (pred n))
                                in
                                  factorial 6"  //This is ans10: NUM 720
    
    ///////////////////// divisor /////////////////////////////////////////////////////////////////////
    //# A PCF program to find the smallest divisor of a number.
    let ans11  = interpstr"let minus = rec m ->
                                fun x -> fun y -> if iszero y then x else m (pred x) (pred y)
                           in
                            let divides = rec div ->      # Does d divide n-1?
                                    fun d -> fun n ->
                                      if iszero n then
                                            false
                                      else if iszero (pred n) then
                                            true
                                      else
                                            div d (minus n d)
                            in
                                let divisor = rec ds ->
                                      fun n -> fun init ->
                                        if divides init (succ n) then
                                          init
                                        else
                                          ds n (succ init)
                                in
                                  divisor 36863 2" //This is ans11: NUM 191 ..... take a long run!
    
    ///////////////// Lists //////////////////////////////////////////////////////////////////////////////////
    //# Here we show that, quite surprisingly, we can implement *lists* in PCF
    //# by using *first-class functions*.  The basic idea is that a list is
    //# represented by a function f, where f 0 returns the head, f 1 returns
    //# the tail, and f 2 returns true if the list is empty and false otherwise.
    let ans12  = interpstr" let cons = fun x -> fun xs -> fun n ->
                              if iszero n then x else if iszero (pred n) then xs else false
                            in
                              let nil = fun n -> true   # This is flawed; hd nil and tl nil both return true!
                              in
                                let hd = fun f -> f 0
                                in
                                  let tl = fun f -> f 1
                                  in
	                                let null = fun f -> f 2
	                                in
	                                  let equal = rec e ->	# This tests whether two integers are equal.
		                                fun a -> fun b -> if iszero a then
			                              iszero b
		                                else if iszero b then
			                              false
		                                else
		                                  e (pred a) (pred b)
	                                  in
	                                    let member = rec m ->
		                                  fun n -> fun ns -> if null ns then
				                                 false
				                               else if equal n (hd ns) then
				                                 true
				                               else
				                                 m n (tl ns)
	                                    in
	                                      member 4 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))"
    //# The output of this program is, of course, BOOL true. Unfortunately,
    //# lists can't be displayed nicely---they print out as huge FUN terms...
    // In this case we got: This is ans12: BOOL true
    
    //# Ackermann's function
    let ans13  = interpstr"let Ack = rec A ->
                              fun x -> fun y ->
                                if iszero x then
                                  succ y
                                else if iszero y then
                                  A (pred x) 1
                                else
                                  A (pred x) (A x (pred y))
                            in
                              Ack 3 6"  //This is ans11: NUM 509
    
    ////printing all ans!
    //printfn "\nThis is ans  : %A" ans
    //printfn "\nThis is ans1 : %A" ans1
    //printfn "\nThis is ans2 : %A" ans3
    //printfn "\nThis is ans3 : %A" ans4
    //printfn "\nThis is ans4 : %A" ans5
    //printfn "\nThis is ans6 : %A" ans6
    //printfn "\nThis is ans7 : %A" ans7
    //printfn "\nThis is ans8 : %A" ans8
    //printfn "\nThis is ans9 : %A" ans9
    //printfn "\nThis is ans10: %A" ans10
    printfn "\nThis is ans11: %A" ans11  //This one take a while to run but at the end the answer is NUM 191
    //printfn "\nThis is ans12: %A" ans12  //This one also take a while to run.
    //printfn "\nThis is ans13: %A" ans13  //also a long run!
    ()