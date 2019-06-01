

//global declaration
let x = 6
x + x

//defined a new x
//let x = x + 1


//local declaration
let y = 17 in 3*y
// y this is an error ..... value or constructor not defined.

// "Let" is NOT assignment!!!! 
// example
let z = 5 in (let z = 3 * z + 1 in 2 * z) + z  
// z is 16 in 2*z, it has value of 32. 
// When we leave the scope of the inner let, z is still 5 so we add and get 37.

let succ1 n = n + 1
succ1 12      // n = 12 + 1 = 13
succ1 3 * 7   // because no parentheses (succ 3) * 7 = 28
succ1 (3 * 7) // 3 * 7 = 21 and n = 21 + 1 = 22

succ1 -12       // negative literal

let succ2 = fun n -> n + 1  // lambda function

// recursion
let rec fact n = if n = 0 then 1 else n * fact (n-1)
fact 5

(* Comments more of one line *)
// Comments in one line
/// Comments recognized by documentation software


// Testing Fibonacci
let rec fib = function
|0 -> 0I
|1 -> 1I
|n -> fib(n-1) + fib(n-2)

fib 12

/////Video Curried Functions
//example 1

let add(a,b) = a + b //val add : a:int * b:int -> int
add(3,4) //val it : int = 7
//add 3 if I try this then I got an error because add should be two arguments.

add(55,5) // int=60

//Using Lambda Functions
let cadd a = (fun b -> a + b)
cadd 3
//it 4


let cadd2 a b = a + b

//////Video List
[1;2;3]

//empty list
[] //val it : 'a list   in this case a list of something

[1..10] //val it : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

/////head and tail examples
List.head [1;2;3]  //val it : int = 1
List.tail [1;2;3]  //val it : int list = [2; 3]
List.length [1;3;9]

// List.map applies a function to all elements of a list and return the list of result
List.map (fun n -> n*n) [1;2;3;4] //val it : int list = [1; 4; 9; 16]

// List.filter Applies predicate to each element of a listand returns the list of 
// elements that satisfy the predicate
List.filter(fun x->x/2 = 0) [1;2;3;4;5]

List.map (fun xs -> 5::xs) [[1;2];[3]] //val it : int list list = [[5; 1; 2]; [5; 3]]
List.map ((*) 7) [1..5] //val it : int list = [7; 14; 21; 28; 35]

// video using list in function 1/19/2019

List.rev []  // error FS0030: Value restriction. this error is fixed with a type annotation.
List.rev ([] : int List) //val it : int list = []  

[1;2] = [1;2] // val it : bool = true because they have same length and same values in each slot.
[1;2;3] = [1;3] //val it : bool = false

[1;2] < [1;2] //val it : bool = false
//we can compare in dictionary order and we can compare tuples of different lengths
[9] < [1;2;3] //val it : bool = false 

//DownFrom example from video "composition and Piping"
let rec downFrom = function
    | 0 -> []
    | n -> n :: downFrom (n - 1)
    //val downFrom : _arg1:int -> int list

downFrom 5 //val it : int list = [5; 4; 3; 2; 1]


//forward pipeline operator.
(|>) //val it : ('a -> ('a -> 'b) -> 'b)

/////////////////// 1/28/2019 ////////////////////////////

[2;4] @ [3;5]

// function recursive to print a list of int
let rec PrintList list = 
    match list with
    | [] -> ()
    | head::tail -> printfn "%d" head 
                    PrintList tail
    
let numbers = [1;2;3;4;5;6;7;8;9]
PrintList numbers

// combine two list
let colors1 = ["blue";"orange"]
let colors2 = List.append colors1 ["red"]
printfn "%A" colors1  //val colors1 : string list = ["blue"; "orange"]
printfn "%A" colors2  //val colors2 : string list = ["blue"; "orange"; "red"]

// also I can use @ to combine two list
let colors3 = colors1 @ ["green"]
printfn "%A" colors3  //val colors3 : string list = ["blue"; "orange"; "green"]

// using List.map to add 1 to each element in List
let ListADD1 = List.map (fun x -> x + 1)
let items = [1;3;5;7;9]           
let itemsfinal = ListADD1 items    
printfn "%A" items                //val items : int list = [1; 3; 5; 7; 9]
printfn "%A" itemsfinal           //val itemsfinal : int list = [2; 4; 6; 8; 10]

//sort a list
let ListSort = List.sort [100; 50; 80; 5; 20; 1; 30] //val ListSort : int list = [1; 5; 20; 30; 50; 80; 100]

//now lets double the element in that list and then sort it
let Double_and_Sort_List list = 
    list
    |> List.map (fun item -> item * 2)
    |> List.sort 

let items1 = [1;3;5;7;9]
let result1 = Double_and_Sort_List items1
printfn "%A" items1   //val items1 : int list = [1; 3; 5; 7; 9]
printfn "%A" result1  //val result1 : int list = [2; 6; 10; 14; 18]

    
///////////////////// 1/29/2019 /////////////////////////////////
// working on problem set 1
// (question # 9)
(fun f -> f 17)  //val it : f:(int -> 'a) -> 'a  

// (question 8)
let rec foo = function                     // 'a list * 'a list -> 'a list
    | (xs, []) -> xs
    | (xs, y :: ys) -> foo(xs @ [y], ys)   

let PrintApendSum = 
    let listA = [1;2;3]
    let listB = [4;5;6]
    let finalList = foo (listA, listB) 
    printfn " The Append of list %A and %A is: %A" listA listB finalList 
    //The Append of list [1; 2; 3] and [4; 5; 6] is: [1; 2; 3; 4; 5; 6]
()

//(question 10)
fun x -> x::[5]  //int -> int list

///////////////////////////// 1/30/2019 ///////////////////////////////////////
// find the last element in a list
let myLast xs = xs|> List.rev|> List.head
let y = myLast [3;5;7;9] //val y : int = 9

let myReverse xs = xs|> List.rev
let ys = myReverse [2;4;6;8]

// List is Palindrome?
let palindrome xs = xs = List.rev xs
let yss = palindrome ["a";"b";"c"] //val yss : bool = false
let ysss = palindrome [1;2;3;2;1]  //val ysss : bool = true


// compare two list element by element.... (((quiz question))) , now I know where was my mistake...
let rec max xs ys =
        match xs, ys with
        | [], ys -> ys     // if only ys then result is ys
        | xs, [] -> xs     // if only xs then result is xs
        | x::xs, y::ys -> if  x > y then x::max xs ys 
                          else y::max xs ys

let list1 = [4;5;1;7]
let list2 = [8;2;6]
let list3 = max list1 list2 // val list3 : int list = [8; 5; 6; 7]


////////////////// 02/04/2019 /////////////////////////////////////
// working on quiz1 solutions
// question 7 function take list of list  xs and remove the first element in all sublist

// solution 1 (Professor solution)
let headless xs = List.map List.tail xs //val headless : xs:'a list list -> 'a list list
let s1 = [[1;2;3];[8];[5;9]]            //val s1 : int list list = [[1; 2; 3]; [8]; [5; 9]]
let s1Final = headless s1               //val s1Final : int list list = [[2; 3]; []; [9]]
printfn "The original list is %A the result of headless is %A " s1 s1Final
//The original list is [[1; 2; 3]; [8]; [5; 9]] the result of headless is [[2; 3]; []; [9]] 

//solution 2 (second professor solution)
let rec map f = function
    |[] -> []
    |x::xs -> f x :: map f xs

//solution 3
let rec headlesss xs = 
    match xs with
    | [] -> []
    | x::xs -> List.tail x :: headlesss xs

let s3 = [[1;2;3];[8];[5;9]]         //val s3 : int list list = [[1; 2; 3]; [8]; [5; 9]]   
let s3Final = headless s3            //val s3Final : int list list = [[2; 3]; []; [9]]   
printfn "The original list is %A the result of headless is %A " s3 s3Final
//The original list is [[1; 2; 3]; [8]; [5; 9]] the result of headless is [[2; 3]; []; [9]]

//////////////////// 02/05/2019 /////////////////////////////////////////////

// function take list of list xs and only take the first element in all sublist and delete the rest.

let rec onlyHead xs = 
    match xs with
    | [] -> []
    | x::xs -> List.head x :: onlyHead xs

let s4 = [[1;2;3];[8];[5;9]]    //val s4 : int list list = [[1; 2; 3]; [8]; [5; 9]]         
let s4Final = onlyHead s4       //val s4Final : int list = [1; 8; 5]       
printfn "The original list is %A the result of onlyHead is %A " s4 s4Final
//The original list is [[1; 2; 3]; [8]; [5; 9]] the result of headless is [1; 8; 5] 

// remove a element in a List given the position.
let rec remove i l =  // i position, l list
    match i, l with
    | i, [] -> failwith "index out of range"
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    

let s5 = [1;2;3;8;5;9]         //val s5 : int list = [1; 2; 3; 8; 5; 9]        
let s5Final = remove 3 s5      //val s5Final : int list = [1; 2; 3; 5; 9]    
printfn "The original list is %A the result of headless is %A " s5 s5Final
//The original list is [1; 2; 3; 8; 5; 9] the result of headless is [1; 2; 3; 5; 9] 

// insert element in a List given the position
let rec insert v i l =  // v new element, i index, l list
    match i, l with
    | i, [] -> failwith "index out of range"
    | 0, xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    
let s6 = [1;2;3;8;5;9]           //val s6 : int list = [1; 2; 3; 8; 5; 9]   
let s6Final = insert 7 0 s5      //val s6Final : int list = [7; 1; 2; 3; 8; 5; 9]
printfn "The original list is %A the result of headless is %A " s6 s6Final
//The original list is [1; 2; 3; 8; 5; 9] the result of headless is [7; 1; 2; 3; 8; 5; 9] 

//problem 17
let revlist xs = List.map List.rev xs    //val revlist : xs:'a list list -> 'a list list
let s7 = [[1;2;3];[8;5];[9]]             //val s7 : int list list = [[1; 2; 3]; [8; 5]; [9]]
let revS7 = revlist s7                   //val revS7 : int list list = [[3; 2; 1]; [5; 8]; [9]]
printfn "original list %A and reverse list %A" s7 revS7 
//> original list [[1; 2; 3]; [8; 5]; [9]] and reverse list [[3; 2; 1]; [5; 8]; [9]]

//problem 23
let powerset1 x =
    let rec ps xs =
        match xs with 
        | []    -> [[]]
        | x::xs -> [x]::List.map (fun y -> [x;y]) xs @ ps(xs)
    x :: ps x 


let l1 = [3;5]
let powerS1 = powerset1 l1 //val powerS1 : int list list = [[3; 5]; [3]; [3; 5]; [5]; []]


// exercice from test1 interleave two list but always bigger element first.
let rec interleave_first xs ys =
    match xs, ys with
    | [],ys  -> ys
    | xs,[] -> xs
    | x::xs,y::ys -> if x > y then x::y::interleave_first xs ys
                                else y::x::interleave_first xs ys

let listA = [3;5;9;2]
let listB = [6;1;3;7]
let listC = interleave_first listA listB // Solution: val listC : int list = [6; 3; 5; 1; 9; 3; 7; 2]

////////////////////////// coding # 2 ////////////////////////////////////////////////////////

//Problem 1
// this is only type int!.... in code.fs is done for general type.
type MyCoordinates = Tuple of int * int | Threeple of int * int * int | Fourple of int * int * int * int


let Fun (tuple, tfunction) = 
   match tuple with
   | Tuple (w,x) -> tfunction w x
   | Threeple (w,x,y) -> tfunction(tfunction w x) y
   | Fourple (w,x,y,z) -> tfunction(tfunction(tfunction w x) y) z 
// val Fun : tuple:MyCoordinates * tfunction:(int -> int -> int) -> int


    // Value restriction
let lst = (fun x -> [x]) in (lst 12, lst true,lst "cat")
    //val it : int list * bool list * string list = ([12], [true], ["cat"])

let x = List.rev [] in 3::x //val it : int list = [3]
    
    // trying Imperative Factorial
let factorial n = //val factorial : n:int -> int
    let ans = ref 1
    let cnt = ref 1
    while !cnt <= n do
        ans:= !ans * !cnt
        cnt:=  !cnt + 1
    !ans

// trying Pointers and Aliases
let r = ref 17     //val r : int ref = {contents = 17;}
let s = r          //val s : int ref = {contents = 17;}
let t = ref (!r)   //val t : int ref = {contents = 17;}

r:= !r + 1         //val it : unit = ()
!s
!t

////////////////////////////////////// 2/28/2019 /////////////////////////////////////////////
// problem 4 set 2
// Recall that an F# function that takes two arguments can be coded in either
// uncurried form (in which case it takes a pair as its input) or curried form
// (in which case it takes the first argument and returns a function that takes
// the second argument).

// curry function
let curry myfunction x y = myfunction(x,y)         //val curry : myfunction:('a * 'b -> 'c) -> x:'a -> y:'b -> 'c
// uncurry
let uncurry myfunction (x,y) = myfunction x y      //val uncurry : myfunction:('a -> 'b -> 'c) -> x:'a * y:'b -> 'c
// Testing
let p = uncurry (+)     //val p : (int * int -> int)
let p1 = p(4,6)         //val p1 : int = 10
printfn "%A" p1         //10

//Testing
let p2 = curry p        //val p2 : (int -> int -> int)
let p3 = p2 3           //val p3 : (int -> int)
let p4 = p3 10          //val p4 : int = 13 
printfn "%A" p4         //13


// Curry function 
let curry a = (fun x -> fun y -> a(x,y)) //val curry : a:('a * 'b -> 'c) -> x:'a -> y:'b -> 'c

// Another way
let curry a x y = a(x,y) //val curry : a:('a * 'b -> 'c) -> x:'a -> y:'b -> 'c

//uncurry
let uncurry a = (fun (x,y) -> a x y) //val uncurry : a:('a -> 'b -> 'c) -> x:'a * y:'b -> 'c

// also
let uncurry a (x,y) = a x y //val uncurry : a:('a -> 'b -> 'c) -> x:'a * y:'b -> 'c

//////////////////////////// 3/3/2019 //////////////////////////////////////////////////////////////
// Problem 7

let rec oddeven = function
| [] -> []                           // O(1) constant
| x::xs -> if x % 2 = 0              // O(1) constant
           then oddeven xs @ [x]     // O(n^2) 
           else x :: oddeven xs      // O(n)
                                     // In general is a O(n^2)
// val oddeven : _arg1:int list -> int list

///////////////////////////// 3/4/2019 /////////////////////////////////////////////////////////////
// 11. Record
//Create a record type for Name, Credits and GPA.
type record = {Name: string; Credits: int; GPA: float }

//Create a record instance with the values "Jones", 109, 3.85.
let student1 = {Name = "Jones"; Credits = 109; GPA = 3.85}

/////////////////////////// 3/18/2019 ////////////////////////////////////////////////////////////
// quiz2 
// problem 5.
type record1 ={age: int; gpa: float; class1: string}
let alum = {age= 20; gpa= 3.1; class1="senior"}

////////////////////////// 3/19/2019 /////////////////////////////////////////////////////////////
//recursive functions, from class examples
let rec split = function
    | []       -> ([],[])
    | [a]      -> ([a],[])
    | a::b::cs -> let (M,N) = split cs
                  (a::M, b::N)
// val split : _arg1:'a list -> 'a list * 'a list

////////////////////////// 3/20/2019 //////////////////////////////////////////////////////////////
let rec factail acc lst =
    match lst with
    | [] -> acc
    | [x] -> acc * x
    | x::xs -> factail (acc * x) xs
// val factail : acc:int -> lst:int list -> int

let solution1 = factail 2 [1;2;3] //val solution1 : int = 12





/////////////////////////// exam 2 testing.... /////////////////////////////////////////////

// pivot question
let list1 = [6;3;8;9;4;5;7;1;2]
let pivot = list1.Head // val pivot : int = 6

let rec split1 pivot = function
       | []    -> ([],[])
       | x::xs -> let (left,right) = split1 pivot xs
                  if x < pivot then (x::left, right)
                               else (left, x::right)

let result = split1 pivot list1
printfn"%A" result  // val result : int list * int list = ([3; 4; 5; 1; 2], [6; 8; 9; 7])


// more on exam 2
let a = 2.0
let b = 8.0
let c = 4.0
let result1 = sqrt(b*b - 4.0*a*c)  // val result1 : float = 5.656854249
let result2 = 2.0*a //val result2 : float = 4.0
let roots (b,result1,result2) = ((-b+result1)/result2, (-b-result1)/result2)
let result3 = roots(b,result1,result2) // val result3 : float * float = (-0.5857864376, -3.414213562)
printfn "%A" result3

// reverse list tail recursive
let reverse1 list =
    let rec loop tmp = function
        |[] -> tmp
        |[x] -> x::tmp
        |x::xs -> loop (x::tmp) xs
    loop [] list
// val reverse1 : list:'a list -> 'a list

let list1 = [6;3;8;9;4;5;7;1;2]
let list2 = reverse1 list1         //val list2 : int list = [2; 1; 7; 5; 4; 9; 8; 3; 6]


//////////////////////////////////// coding 3 ////////////////////////////////////////////////////////////////

//question 5 set 3
let interleaves_TailRecursive xs ys =
    let rec loop xs ys tmp =
        match xs, ys with
        |[],[] -> tmp
        |[], _ -> failwith "List lengths Error!!!"     
        |_, [] -> failwith "List lengths Error!!!"
        |x :: xs, y :: ys -> loop xs ys (y::x::tmp) 
    loop xs ys [] |> List.rev
//val interleaves_TailRecursive : xs:'a list -> ys:'a list -> 'a list

let list1 = [2;4;6]                                 // val list1 : int list = [2; 4; 6]
let list2 = [3;5;7]                                 // val list2 : int list = [3; 5; 7]
let list3 = interleaves_TailRecursive list1 list2   // val list3 : int list = [2; 3; 4; 5; 6; 7]

///////////////////////////////////// 3/31/2019 //////////////////////////////////////
// problem 8
//   Create a tail-recursive function that has a big integer as input and calculates 2I raised to that power.
//   Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.

let raise2I x =
    let rec loop x acc =
        match x with
        |_ when x = 0I -> acc
        |n -> loop (x - 1I) (acc * 2I)
    loop x 1I
// val raise2I : x:System.Numerics.BigInteger -> System.Numerics.BigInteger

let x1 = raise2I 0I      // val x1 : System.Numerics.BigInteger = 1
let x2 = raise2I 1I      // val x2 : System.Numerics.BigInteger = 2
let x3 = raise2I 2I      // val x3 : System.Numerics.BigInteger = 4
let x4 = raise2I 4I      // val x4 : System.Numerics.BigInteger = 16
let x5 = raise2I 16I     // val x5 : System.Numerics.BigInteger = 65536
let x6 = raise2I 256I    // val x6 : System.Numerics.BigInteger = 115792089237316195423570985008687907853269984665640564039457584007913129639936
let x7 = raise2I 1024I
let x8 = raise2I 32768I
let x9 = raise2I 65536I

///////////////////////////////////////// 4/7/2019 ////////////////////////////////////
// problem2 set3
// This CFG recognizes some strings of zeros and ones.
//        S → 0A | 1B  
//        A → 0AA | 1S | 1
//        B → 1BB | 0S | 0         

// Describe the strings that the CFG recognizes. This language is ambiguous. Find a string that is recognized by 
// this grammar which has two derivations. Show the two derivation trees for the string in part (b).

// CFG recognizes combinations of 00 01 10 11..... the length must be even...... could be '(' ')' ?
// 0 is '(' and 1 is ')' ???


// 001101

// S -> 0A -> 00AA -> 0011
// S -> 1B -> 11BB -> 1100
// S -> 0A -> 00AA -> 001S1 -> 0011B1 -> 001101

//       S
//      0 A
//     0 0 A A
//    0 0   1 1S
//   0 0     1  10A
//  0 0       1    101 -> 001101



////////////////////////////////////////////////////////////////////////////////////////////////////////// 
//problem10 set3
// List the steps that F# follows to determine the type for f: (fun f -> f (f 17.3)).

(fun f -> f (f 17.3)) // val it : f:(float -> float) -> float

// fun f = 'a -> 'b
// fun f -> f = 'a -> 'a
// fun f -> f (f) = ('a -> 'a) -> 'a
// fun f -> f (f 17.3) (float -> float) -> float

///////////////////////////////////////// 4/11/2019 //////////////////////////////////////////////////////
//problem9 set3

let twice f = f << f    // val twice : f:('a -> 'a) -> ('a -> 'a)
let succ n = n + 1      // val succ : n:int -> int

(twice (twice (twice (twice (twice succ))))) 0  // val it : int = 32

twice twice twice succ 0   // val it : int = 16
// 2 -> 2**2 -> 2**2**2 = 16

twice twice twice twice succ 0 //val it : int = 65536
// 2**2**2**2 = 65536

let twice f = f << f;; //val twice : f:('a -> 'a) -> ('a -> 'a)
 
let successor n = n+1;; //val successor : n:int -> int
 
let rec questionNine k f g =
    if k > 1 then questionNine (k-1) (f<<twice) g
    else  (fun a -> f g a)
// val questionNine : k:int -> f:(('a -> 'a) -> 'b -> 'c) -> g:('a -> 'a) -> ('b -> 'c)




//////////////////////////////////////////////////////////////////////////////////////////////////////////

// All odd numbers in finite set 
let odds n = Seq.where(fun x -> (x % 2 <> 0)) n  //val odds : n:seq<int> -> seq<int>
// Example returns a sequence of odd numbers from list with elements from 0 to 10 
odds [0 .. 10] ;;  //val it : seq<int> = seq [1; 3; 5; 7; ...]

// An infinite sequence of natural numbers 
let nat = Seq.initInfinite(fun x -> x)     //val nat : seq<int>

// Infinite sequence of odd numbers 
let uneven = Seq.filter(fun x -> (x % 2 <> 0)) nat  // val uneven : seq<int>

// Find n=1000 odd numbers in infinite sequence of odd numbers 'uneven'
Seq.toList(Seq.take 15 uneven) // val it : int list = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29]


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//problem 14
//(LETVAR) x: int* var
//(LIT) 15: int
//a: int*
//Then
//(LIT) 7:int
//(R-VAL) x:int * 
//(L-VAL) *x: int var
//(ASSIGN) *x = 7: int var = int

//(R-VAL)(L-VAL)(R-VAL) *x: int

//(L-VAL) (a + *x): int var
//(LIT) 4:int
//(ADD) *x + 4: int
//(ASSIGN) (a[*x] = *x + 4): int var = int

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Problem 4
// a) ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
//  j in Domain(M) j(M)=8, (2,M)=2
//  ({i=5; j=8}, 2*j) = 16
//  i in Domain (M) i(M)=5
//  ({i=5; j=8}, 2*j + i) = 21
//  ({i=5; j=8}, i := 2*j + i) =21
//  ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}

//b) ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
//  i in domain (M) i(M)=3 , (2,M)=2
//  ({i=3; j=8}, 2*i)=6
//  j in domain (M), j(M)=8 
//  ({i=3; j=8}, 2*i > j) = False
//  i in domain (M) i(M)=3 , (2,M)=2
//  ({i=3; j=8}, j:= 2*i) => {i=3; j=6}
//


/////
//testing stuff......
seq { for i in 1 .. 10 do yield i * i } //val it : seq<int> = seq [1; 4; 9; 16; ...]



// Recursive isprime function.
let isprime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i + 1))
    check 2
//val isprime : n:int -> bool

let aSequence = seq { for n in 1..100 do if isprime n then yield n }
//val aSequence : seq<int>
for x in aSequence do
    printfn "%d" x //1, 2, 3, 5, ...

//////////////////////////////////////////////////////
