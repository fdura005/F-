module type_example

// PCF Type Inference Skeleton File

// This lets us refer to a constructor like "Parser.Parse.ID" simply as "ID".
open Parser.Parse

type typ = VARIABLE of string | INTEGER | BOOLEAN | ARROW of typ * typ

/// Convert a typ to a string, using as few parentheses as necessary.
let rec typ2str = function
| VARIABLE a -> "'" + a
| INTEGER    -> "int"
| BOOLEAN    -> "bool"
| ARROW (ARROW (t1, t2), t3) -> "(" + typ2str (ARROW (t1, t2)) + ") -> " + typ2str t3
| ARROW (t1, t2) -> typ2str t1 + " -> " + typ2str t2

// Code for handling substitutions, which are functions typ -> typ.

/// Identity substitution.
let I (t : typ) = t

/// unify (t1, t2) returns the most general substitution that
/// unifies types t1 and t2, failing if no such substitition exists.
let rec unify (t1, t2) =
  let rec replace (a, t) = function
  | VARIABLE b     -> if a = b then t else VARIABLE b
  | ARROW (t1, t2) -> ARROW (replace (a, t) t1, replace (a, t) t2)
  | t1             -> t1
  
  let rec occurs = function
  | (a, VARIABLE b)          -> (a = b)
  | (a, ARROW (t1, t2))      -> occurs (a, t1) || occurs (a, t2)
  | (a, _)                   -> false
  
  match (t1, t2) with
  | (VARIABLE a, t) -> 
      if t = VARIABLE a then I                  //noting to do
      elif occurs (a, t) then                   //causes infinite recursion
        failwith (sprintf "circularity: cannot unify %A and %A" a t)
      else replace (a, t)                       //subsitute all Variable a with t
  | (t, VARIABLE a)    -> unify (VARIABLE a, t) //a is a variable, t is not, unify t to a
  | (INTEGER, INTEGER) -> I                     //nothing to do
  | (BOOLEAN, BOOLEAN) -> I                     //nothing to do
  | (ARROW (t3, t4), ARROW (t5, t6)) ->         
      let s1 = unify (t3, t5)                   //unify left side
      let s2 = unify (s1 t4, s1 t6)             //unify right with new left
      s2 << s1                                  //compose s1 to s2
  | _ -> failwith (sprintf "mismatch: cannot unify %A and %A" t1 t2)

// Code for handling environments, which are functions string -> typ.

let emptyenv x = failwith ("identifier " + x + " is unbound")

/// update env x t is the same as env, except that x is mapped to t.
let update env (x : string) (t : typ) = fun y -> 
    if y = x then t                             //return new value for x 
    else env y                                  //return value from env

// Code for generating new type variables.

// We start by building the infinite stream of all type variables.
type 'a stream = Cons of 'a * (unit -> 'a stream)

//type variables only have one letter
let rec upfrom n =
  let letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                 "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]
  Cons (VARIABLE (letters.Item (n % 26) + if n/26 = 0 then "" else string (n/26)),
        fun () -> upfrom (n+1))
     
let alltypevars = upfrom 0

/// newtypevar() generates a new type variable, and reset() resets back to 'a.
let (newtypevar, reset) =                       //returns tuple
  let vars = ref alltypevars                    //mutable
  let hd (Cons (x, xsf)) = x
  let tl (Cons (x, xsf)) = xsf()
  ((fun () ->                                       
      let next = hd (!vars) in vars := tl (!vars)   
      next),                                    //next available var
   (fun () -> vars := alltypevars))             //reset to all vars

/// Milner's Algorithm W
// I have included the code for NUM and IF; you must complete the rest!
let rec W (env, e) = 
  match e with
  | APP (m1, m2)     -> let (s1, t1) = W(env, m1) //Find the type of m1
                        let output = match (t1) with
                                     | ARROW(input, output) -> output
                                     | anything -> anything
                        let (s2, t2) = W(s1 << env, m2) //Find the type of m2
                        
                        let s3 = match (s2 t1) with
                                | ARROW(x, y) -> let a = newtypevar()
                                                 unify(s2 t1, ARROW(t2, a)) //Unify the argument to the function
                                | _ -> unify(s2 t1, t2) //Unify two elements
                        (s3 << s2 << s1, s3 output) //Return the environment, with the modification to the type
  | NUM n           -> (I, INTEGER)
  | BOOL b          -> (I, BOOLEAN)
  | IF (e1, e2, e3) -> let (s1, t1) = W (env, e1) //Find type of e1
                       let s2 = unify (t1, BOOLEAN) //e1 must be a boolean
                       let (s3, t2) = W (s2 << s1 << env, e2) //Find type of e2
                       let (s4, t3) = W (s3 << s2 << s1 << env, e3) //Find type of e3
                       let s5 = unify (s4 t2, t3) //Make sure e2 has the same type as e3
                       (s5 << s4 << s3 << s2 << s1, s5 t3) //Return

  | SUCC            -> (I,ARROW(INTEGER, INTEGER))
  | PRED            -> (I,ARROW(INTEGER,INTEGER))
  | ISZERO          -> (I,ARROW(INTEGER,BOOLEAN))
  | FUN (param,body)-> let a=newtypevar ()
                       let (s,t) = W (update env param a, body)
                       let s2 = unify(s a, t)
                       (s2 << s, ARROW(s2 a,t))
  | REC (name,body) -> let (s1, t1) = W(env, body) //Find type of body of REC
                       (s1, ARROW(t1,t1))

  | ID s            -> (I,VARIABLE s)
  | _               -> failwith"Some case was incomplete"
  
/// infer e finds the principal type of e
let infer e =
  reset ();
  let (s, t) = W (emptyenv, e)
  printf "The principal type is\n %s\n" (typ2str t)

let test () =
    //infer (NUM 12)
    //infer (BOOL true)
    //infer (IF(BOOL true, NUM 1, NUM 2))
    //infer (IF(BOOL true, IF(BOOL true, NUM 1, NUM 2), IF(BOOL false, NUM 3, NUM 4)))
    //infer (APP(SUCC,NUM 5))
    //infer (IF(APP (ISZERO, NUM 0), APP (SUCC, NUM 5), APP (PRED, NUM 50)))
    //infer (IF(APP (ISZERO, NUM 0), APP (ISZERO, NUM 5), APP (ISZERO, NUM 50)))

    //infer (APP(SUCC,NUM 5))
    //infer (APP(PRED,NUM 5))


    //infer (FUN ("n", NUM 7))
    //infer (FUN ("x", ID "x"))
    //infer (FUN ("x", FUN ("y", ID "x")))
    //infer (FUN ("n", APP (SUCC, ID "n")))
    //infer (FUN ("x", FUN ("y", APP (SUCC, ID "x"))))

    //infer (REC ("m", FUN ("x", FUN ("y", IF( APP(ISZERO, ID "y"), ID "x", APP (APP (ID "m", APP (PRED, ID "x")), APP (PRED, ID "y")))))))
    ()
