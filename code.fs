module code
open System

module Problems1 =
    
    
    printf "P1: Enter an integer number: "

    let input = Console.ReadLine()
    let n = input|> int
    let pred = n - 1
    let succ = n + 1
    
    let test () =
        printfn " Succ of %s is %d" input (succ)
        printfn " Pred of %s is %d" input (pred) 
        ()

module Problems2 =

    printf "P2: Enter a List of integers: "

    let head = function
    | [] -> failwith " Empty list does not have a head ..."
    | x::xs -> x


    let test () =
        printfn " the head of [1;2;3] is %d" (head [1;2;3])
        printfn " the head of [1] is %d" (head [1])  
        ()

module Problems3 =
    
    let rec fib = function
        |0 -> 0
        |1 -> 1
        |n -> fib(n-1) + fib(n-2)
    
    
    let test () =
       printfn " The fibonacci number of 15 is : %A"  (fib 15)
       printfn " The fibonacci number of 20 is : %A"  (fib 20)
       ()

module square_number =
   
    // This is how to square a value
    printf " Enter an integer number to square it: "
    
    let number = Console.ReadLine()
    let input  = number |> int
    let square_value = (input * input)

    let test () =
        printfn " The square of %s is : %d"  number (square_value)
        ()


module List_prod =
// multiply together all nombers in a list
    let rec prod ns =
        if List.isEmpty ns then 1
        else List.head ns * prod (List.tail ns)

    //Same as before but using function keyword to perform pattern matching.
    let rec prod1 = function
        | [] -> 1  // pattern [] matches only with empty list and output is 1
        | n1::ns1 -> n1 * prod ns1

    //This time we are using matching via an explicit match expression
    // have to keep in mind that ms is explicit parameter that is only use to match. 
    let rec prod2 ms = 
        match ms with
        | [] -> 1  // pattern [] matches only with empty list and output is 1
        | n2::ns2 -> n2 * prod ns2

    let test () =
        printfn " The prod of the List [1..5] is : %d"  (prod [1..5])
        printfn " The prod of the List [1..4] is : %d"  (prod1 [1..4])
        printfn " The prod of the List [1..3] is : %d"  (prod2 [1..3])
        ()

module Problems16 =
    // gcd Calculate the greatest common divisor
    let rec gcd = function
        | (a,0) -> a
        | (a,b) -> gcd (b,a%b)
    
    // addition of two fraction    
    let (.+) (a,b) (c,d) = 
        let x = (a*d)+(b*c)
        let y = (b*d)
        let z = gcd(x,y)
        (x/z,y/z)
    
   
    let addFraction () =
        let x = (1,2)
        let y = (1,3)
        let (a,b) = x .+ y
        printfn " The addition of %A and %A is: (%A,%A) " x y a b 
        printfn " "       
        ()

    // multiplication of two fraction    
    let (.*) (a,b) (c,d) = 
        let x = (a*c)
        let y = (b*d)
        let z = gcd(x,y)
        (x/z,y/z)
    
   
    let multFraction () =
        let x = (1,2)
        let y = (1,3)
        let (a,b) = x .* y
        printfn " The multiplication of %A and %A is: (%A,%A) " x y a b 
        printfn " "       
        ()

    //Problem 17 from Problem set 1
    let revlists xs = List.map (fun x -> List.rev x) xs  

    let reverseExample() = 
        let list1 = [[0;1;1];[3;2];[];[2;4]] // Original List
        let x = revlists[[0;1;1];[3;2];[];[2;4]] //call revlists and reverse it
        // print the 2 Lists.... Original and reverse.
        printfn "The reverse of %A is %A" list1  x
        ()

module Problem18_interleave =
    // problem 18 from problem set 1... function that interleave two list
    let rec interleave xs ys =
        match xs, ys with
        | [], ys -> ys
        | xs, [] -> xs
        | x::xs , y::ys -> x::y::interleave xs ys // in list put x then y and call again interleave
        
        
    let test () =
        let list1 = [1;2;3]
        let list2 = [4;5;6]
        let listFinal = interleave list1 list2 
        printfn " The interleave of %A and %A is : %A" list1 list2 listFinal
        ()   

    //Sum element of two list and put the result in a new list.
    let rec sumList xs ys =
        match xs, ys with
        | [], ys -> ys     // if only ys then result is ys
        | xs, [] -> xs     // if only xs then result is xs
        | x::xs, y::ys -> (x + y)::sumList xs ys // sum first element of each list and call function sumList again

    let printSum () =
        let list1 = [1;2;3]
        let list2 = [4;5;6]
        let listSum = sumList list1 list2
        printfn " The Sum of element of list %A and %A is: %A" list1 list2 listSum 
        ()


module Problem19_CUT =
    // gencut is a function to cut a list in two where n give us the size of the first list. 
    let gencut (n,list) = 
        let rec aux = function
            | 0, xs, ys -> (List.rev xs, ys)
            | n, xs, [] -> (xs,[])
            | n, xs, ys -> aux (n - 1, List.head ys :: xs, List.tail ys)
        aux(n,[],list)

     
    let test_gencut () =
        let n = 2
        let A = [1;2;5;2;2;7;9;3]
        let B = gencut (n,A)
        printfn " A cut of %A with size %d is : %A" A n B
        ()

    let cut list = 
        let n = (List.length list)/2
        gencut(n,list)

    // cut is a function that cut one list in two equal part
    let print_CUT () = 
        let list = [1;2;5;6;8;7;9;3]
        let cutList = cut list
        printfn "The list %A in two equal list is %A" list cutList
        ()


module Problem20_shuffle =
    // function that interleave two list
    let rec interleave xs ys =
        match xs, ys with
        | [], ys -> ys
        | xs, [] -> xs
        | x::xs , y::ys -> x::y::interleave xs ys // in list put x then y and call again interleave

 
    // gencut is a function to cut a list in two where n give us the size of the first list. 
    let gencut (n,list) = 
        let rec aux = function
            | 0, xs, ys -> (List.rev xs, ys)
            | n, xs, [] -> (xs,[])
            | n, xs, ys -> aux (n - 1, List.head ys :: xs, List.tail ys)
        aux(n,[],list)
    
    // cut is a function that cut the list in two equal-sized pieces    
    let cut list = 
        let n = (List.length list)/2
        gencut(n,list)
    
    // function shuffle takes a even list, cut it in two equal sized pieces, 
    // and then interleaves the pieces
    let shuffle list =
        let (x,y) = cut list
        interleave x y

    // Print shuffle
    let print_shuffle () =
        let list = [1;2;3;4;5;6;7;8;9;10]
        let newList = shuffle list
        printfn "The list: %A is shuffle to : %A " list newList

        ()

////////////////////////// coding # 2 ////////////////////////////////////////////////////////

module Problem1_Set2 =
//  Discriminated Union

    // a) Create a discriminated union for Coordinates that can be a Tuple, Threeple or Fourple 
    //    that represent tuples of size two, three and four. The type for the union should be polymorphic.
    type my_coordinates<'x> =
        | Tuple of 'x* 'x
        | Threeple of 'x* 'x* 'x
        | Fourple of 'x* 'x* 'x* 'x
        
    // b)  Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.       
    let tuple1 = Tuple(4,8) //val tuple1 : my_coordinates<int> = Tuple (4,8)
    let tuple2 = Tuple(3,6)

    let threeple1 = Threeple(1.0,2.0,3.0) //val threeple1 : my_coordinates<float> = Threeple (1.0,2.0,3.0)
    let threeple2 = Threeple(4.0,5.0,6.0)

    let fourple1 = Fourple("Testing ", "my ", "code ", "now!...") // val fourple1 : my_coordinates<string> =
                                                                  // Fourple ("Testing ","my ","code ","now!...")
    
    // c) Create a function that has a parameter of a binary function and Coordinate.
    //    Apply the function to the Coordinate like List.reduce.
    let Fun (my_coordinates, my_Fun) = 
        match my_coordinates with
        | Tuple(x,y) -> my_Fun x y
        | Threeple(x,y,z) -> my_Fun(my_Fun x y ) z
        | Fourple(x,y,z,w) -> my_Fun(my_Fun(my_Fun x y) z) w
    //val Fun : my_coordinates:my_coordinates<'a> * my_Fun:('a -> 'a -> 'a) -> 'a

    // d) Call the function with (+) for each of the Coordinates in part (b).
    let tuple_1 = Fun (tuple1,(+)) //val tuple_1 : int = 12
    let tuple_2 = Fun (tuple2,(+)) //val tuple_2 : int = 9

    let threeple_1 = Fun (threeple1,(+)) //val threeple_1 : float = 6.0
    let threeple_2 = Fun (threeple2,(+)) //val threeple_2 : float = 15.0

    let fourple_1 = Fun (fourple1,(+)) //val fourple_1 : string = "Testing my code now!..."

    // e) Call the function with (-) for the numeric Coordinates in part (b). 
    //    Be sure that your function implements the normal associativity for (-).
    let tuple1_1 = Fun (tuple1,(-)) //val tuple1_1 : int = -4
    let tuple2_2 = Fun (tuple2,(-)) //val tuple2_2 : int = -3

    let threeple1_1 = Fun (threeple1,(-)) //val threeple1_1 : float = -4.0
    let threeple2_2 = Fun (threeple2,(-)) // val threeple2_2 : float = -7.0

    let test () =
        printfn "%A and %A" tuple1 tuple2  
        printfn "%A and %A" threeple1 threeple2 
        printfn "Fourple is %A" fourple1
        printfn "Calling %A with (+) is: %A" tuple1 tuple_1
        printfn "Calling %A with (+) is: %A" threeple1 threeple_1
        printfn "Calling %A with (+) is: %A" fourple1 fourple_1
        printfn "Calling %A with (-) is: %A" tuple1 tuple1_1
        printfn "Calling %A with (-) is: %A" threeple1 threeple1_1

        ()
 
module Problem2_Set2 =
// problem 2 from set 2 (fixed!!!!)
    type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
    
    

    let eat token = function
        | [] -> failwith "error!!!...."
        | x::xs -> if x = token then xs
                                else failwith (sprintf "error!!!...expected %A, got %A" token x )
    // val eat : token:'a -> xs:'a list -> 'a list when 'a : equality

    let accept () = printfn "Program Accepted!..."  // val accept : unit -> unit
    let error () = printfn "Syntax Error!..."       // val error : unit -> unit

    let rec S token =
        match token with
        | [] -> failwith "error!!!...."
        | x::xs -> match x with
                  | IF -> xs |> eat ID |> eat THEN |> S |> eat ELSE |> S 
                  | BEGIN -> xs |> S |> L 
                  | PRINT -> xs |> eat ID
                  | _ -> failwith "error!!!...."
    and L = function  // val L : _arg2:TERMINAL list -> TERMINAL list
        | [] -> failwith "error!!!...."
        | x::xs -> match x with
                   | END -> xs
                   | SEMICOLON -> xs |> S |> L

 
    
    let test_program program =
            let result = program |> S
            match result with
            | [] -> failwith "error!!!...."
            | x::xs -> if x = EOF then accept() else error()
    // val test : program:TERMINAL list -> unit

    let myProgram = [IF;ID;THEN;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF];
    let myProgram2 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let myProgram3 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]

    //Testing
    let test ()=
        test_program myProgram
        ()

////////////////////// 2/25/2019 ///////////////////////////////////////////////////////////////
     
// Testing examples from class today 2/25
module Examples_Testing =

    let rec map f = function
        | [] -> []
        | x::xs -> f x :: map f xs
    // val map : f:('a -> 'b) -> _arg1:'a list -> 'b list
    
    /////// Inline Functions! ////////
    let inline double x = x + x //val inline double : x: ^a ->  ^b when  ^a : (static member ( + ) :  ^a *  ^a ->  ^b)
    let z = double 2 // val z : int = 4
    let y = double "TWO" // val y : string = "TWOTWO"

    //F# compiler will infer a polymorphic type
    let inline add a b = a + b  // val inline add :  a: ^a -> b: ^b ->  ^c  when ( ^a or  ^b) : (static member ( + ) :  ^a *  ^b ->  ^c)
    let w = add 4 5      //val w : int = 9
    let e = add 3.5 4.2  //val e : float = 7.7

          
    let test () =
        printfn "Testing Inline Function... double of 2 is: %A " z
        printfn "Testing Inline Function... double of TWO is: %A " y
        printfn "ADD of 4 and 5 is: %A " w
       
        ()

////////////////////// 2/26/2019 ///////////////////////////////////////////////////////////////
module Examples_Testing1 =

    // Parsing a string to a Token list from videos on class
    type tokens = ZERO | ONE | EOF | ERROR
    // type tokens =
    //   | ZERO
    //   | ONE
    //   | EOF
    //   | ERROR
    
    let rec parse = function
        | "" -> [EOF]
        | s -> 
            match s.Chars 0 with
            | '0' -> ZERO :: parse (s.Substring 1)
            | '1' -> ONE :: parse (s.Substring 1)
            | c -> failwith (sprintf "Parse: invalid input %A" c)
    //  val parse : _arg1:string -> tokens list

    // this function accepts a token and a list of tokens.
    // if head of the list matches the token, return the tail, otherwise fail
    let eat token = function
        | [] -> failwith "premature termination of input"
        |x::xs -> 
            if x = token
            then xs
            else failwith (sprintf "want %A, got %A" token x)
    // val eat : token:'a -> _arg1:'a list -> 'a list when 'a : equality

    //Function that processes the S production. when 0 is found looks for another S and then eat one.
    //If one or eof are found, end the function and return the original list.
    let rec S = function
        | [] -> failwith "premature termination of input"
        | x::xs -> 
            match x with
            | ZERO -> xs |> S |> eat ONE
            | ONE -> x::xs
            | EOF -> x::xs
            | _ -> failwith (sprintf "S:, want 0 got meny")
    // val S : _arg1:tokens list -> tokens list

    // Check Syntax... Valid input should start with the S non-terminal.
    let checkSyntax = function
        | [] -> false
        | xs ->
            let tokens = xs |> S
            if tokens <> [EOF]
            then printfn "Syntax: want %A, got EOF" tokens 
            tokens = [EOF]

    let catchFail f default_result =
        try f ()
        with
        | Failure msg -> printfn "Error: %A" msg
                         default_result

    //accepts a string input, generates a list of tokens and checks them for correct syntax
    let parse_check s = 
        let tokens = catchFail (fun () -> parse s) [ERROR]
        printfn "Tokens for %A = %A" s tokens
        let out = catchFail (fun () -> checkSyntax tokens) false
        printfn "is valid for %A = %A" out

    //Testing
    let test ()=
        parse_check "0"
        parse_check "1"
        parse_check "2"
        parse_check "00110011"
        parse_check "0011"
        parse_check "0000000011111111"
        parse_check "00000000111111111"
        ()

module prob2_set2 =
// new problem 2 solution..... 
    type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

    let accept() = printfn "good input"
    let error() = failwith "bad input"
    // inputs for testing
    let myProgram = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF];  
    let myProgram2 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let myProgram3 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]


    let eat t = function
        | x::xs -> if x = t then xs else error()
        | _ -> error()
    // val eat : t:'a -> _arg1:'a list -> 'a list when 'a : equality


    let rec S tok =
        match tok with
        | IF::xs -> xs |>eat ID |>eat THEN |> S |>eat ELSE |> S
        | BEGIN::xs -> xs |> S |> L
        | PRINT::xs -> xs |> eat ID
        | _ -> error()
    and L tok =
        match tok with
        | END::xs -> xs
        | SEMICOLON::xs -> xs |> S |> L
        | _ -> error()
    and E tok = eat(ID)


    let test_program program = 
        let result = program |> S
        match result with
        | [] -> failwith "Early termination or missing EOF"
        | x::xs -> if x = EOF then accept() else error()

    //Testing
    let test ()=
        test_program myProgram3
        ()

////////////////////////// 2/28/2019 //////////////////////////////////////////////////////////

module Sort_Example =

    let QuickSort1 lst = 
        let rec QuickSortCont l cont = //val QuickSort1 : lst:'a list -> 'a list when 'a : comparison
            match l with
            | [] -> cont []
            | pivot::rest -> 
            let left, right = rest |> List.partition(fun i -> i < pivot)
            QuickSortCont left (fun accLeft -> 
            QuickSortCont right (fun accRight -> cont(accLeft@pivot::accRight)))
        QuickSortCont lst (fun x -> x)
    
    let test ()=
        let list1 = [3;1;4;1;5;9;2;6;5]
        let list2 = QuickSort1 list1   //val it : int list = [1; 1; 2; 3; 4; 5; 5; 6; 9]
        printfn "Sorting %A in %A" list1 list2
        ()


/////////////////////////// 3/1/2019 /////////////////////////////////////////////////////////

module problem6_set2 =
    // transpose matrix
    let rec transpose matrix = 
        match matrix with
        | row::rows ->
            match row with 
            | col::cols ->
                let first = List.map List.head matrix
                let rest = transpose (List.map List.tail matrix)
                first :: rest 
            | _ -> []
        | _ -> []
    //val transpose : matrix:'a list list -> 'a list list

    let rec innerProd l1 l2 =
        match (l1,l2) with
        | ([],[]) -> 0
        | (x::xs,y::ys) -> (x*y) + innerProd xs ys
        | _ -> failwith "List are not of equal length..."
    //val innerProd : l1:int list -> l2:int list -> int

    let rec matrixMultiplication = function
    | (xs, []) -> []
    | (xs, []::ys) -> []
    | (xs, ys) -> let headsOfY = List.map (fun y -> List.head y) ys
                  let sums = List.map (fun x -> (innerProd x headsOfY)) xs
                  let remaindingYs = List.map (fun k -> List.tail k) ys
                  sums @ matrixMultiplication (xs, remaindingYs)
    //val matrixMultiplication : int list list * int list list -> int list
 
    let rec splitList = function
        | [] -> [], []
        | [x] -> [x], []
        | x1::x2::xs -> let xs1, xs2 = splitList xs
                        x1::xs1, x2::xs2
    //val splitList : _arg1:'a list -> 'a list * 'a list


    let l1 = [1;2;3]
    let l2 = [4;5;6]
    let l3 = [[1;2;3];[4;5;6]]
    let l4 = [[0;1];[3;2];[1;2]]
    let trans = transpose [l1;l2]                  //val trans : int list list = [[1; 4]; [2; 5]; [3; 6]]
    let resultMult = matrixMultiplication (l3,l4)  // val resultMult : int list = [9; 21; 11; 26]

    let test1 () =
        printfn "The transpose of %A and %A is: %A" l1 l2 trans
        printfn "The Multiplication of %A and %A is: %A" l3 l4 (splitList resultMult)
        ()

///////////////////////// 3/3/2018 //////////////////////////////////////////////////////////////////////////////

module problem3_set2 =
    //  E -> E + T | E - T | T
    //  T -> T * F | T / F | F
    //  F -> i | (E)

    type NONTERMINAL= ADD|SUB|ID|MUL|DIV|LPAREN|RPAREN|EOF
    let eat token= function
        |[]-> failwith "Error!..."
        |x::xs -> if x = token then xs
                               else failwith (sprintf "unknown token: %A, expected: %A" x token)
    //val eat : token:'a -> _arg1:'a list -> 'a list when 'a : equality
 
    let rec E = function
        |[]-> failwith "premature termination"
        |x::xs-> match x with
                |ID->xs|>T
                |LPAREN->xs|>E|>eat RPAREN|>T
                |_->failwith (sprintf "unknown token: %A" x)
    //val E : _arg1:NONTERMINAL list -> NONTERMINAL list   
    and T x = 
        match x with
        |[]->  failwith "premature termination"
        |x::xs-> match x with
                |ADD->xs|>E
                |SUB->xs|>E
                |MUL->xs|>E
                |DIV-> xs|>E
                |EOF-> x::xs
                |_->x::xs
    //val T : x:NONTERMINAL list -> NONTERMINAL list

    let accept() = printfn "good input"             // val accept : unit -> unit
    let error()  = failwith "Error!...bad input"    // val error : unit -> 'a

    let test_program program =
      let result = program |> E
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x =  EOF then accept() 
                             else  printfn "unknown token: %A" x 
                                   error()
    // val test_program : program:NONTERMINAL list -> unit

    let test () =
    test_program [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
    test_program [ID;SUB;ID;MUL;ID;EOF]
    test_program [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF] 
    ()


module problem5_set2 =

    let rec curried list1 = 
        let innerProduct list2 = 
            match (list1, list2) with
            | ([], []) -> 0I
            | (x::xs, y::ys) -> if (List.length list1) <> (List.length list2) then failwith "Lists are of different length"
                                                                              else x*y + (curried xs ys)
        innerProduct
    //val curried : list1:System.Numerics.BigInteger list -> (System.Numerics.BigInteger list -> System.Numerics.BigInteger)
 
    let tailRecursive list1 list2 =
        let rec innerProduct list1 list2 accu =
            match (list1, list2) with
            | ([], []) -> accu
            | (x::xs, y::ys) -> if (List.length list1) <> (List.length list2) then failwith "Lists are of different length"
                                                                              else (innerProduct xs ys (accu + (x*y)))
        innerProduct list1 list2 0I
    // val tailRecursive : list1:System.Numerics.BigInteger list -> list2:System.Numerics.BigInteger list -> System.Numerics.BigInteger
 
    let rec nonTailRecursive list1 list2 =
        match (list1, list2) with
        | ([], []) -> 0I
        | (x::xs, y::ys) -> if (List.length list1) <> (List.length list2) then failwith "Lists are of different length"
                                                                          else (x*y) + nonTailRecursive xs ys
    //val nonTailRecursive : list1:System.Numerics.BigInteger list -> list2:System.Numerics.BigInteger list -> System.Numerics.BigInteger

    let test () =
        let result_1 = curried [1I..500I] [501I..1000I]
        let result_2 = curried [1I;2I;3I] [4I;5I;6I] 
        let result_3 = tailRecursive [1I..50000I] [50001I..100000I]    //stack overflow
        let result_4 = nonTailRecursive [1I..50000I] [50001I..100000I] //stack overflow
        printfn "%A" result_2
        ()

/////////////////////////////// 3/4/2019 ///////////////////////////////////////////////////////////////////////////
module problem9_set2 =
    
    // Type
    type option<'a> = None | Some of 'a

    // Write a helper function that converts an option to a string. If the option is None then return "Invalid Input",
    // otherwise use sprintf to convert the value in the option to a string.
    let helper_function (mylist, opt) = 
        match opt with
        | None   -> "Error!....Invalid input"
        | Some a -> sprintf "The last element of %A is %A" mylist a
    // val helper_function : mylist:'a * opt:'b option -> string

    // Write a recursive function that returns the last element in its list parameter, using the option type to handle
    // invalid input. You may not use reduce. Do not reverse the list. Do not index into the list. Use recursion.
    let rec Last_Element mylist originalList =
        match mylist with
        | []    -> helper_function (mylist, None)
        | x::xs -> if xs.IsEmpty then helper_function (originalList, Some(x))
                                 else Last_Element xs originalList
    // val Last_Element : mylist:'a list -> originalList:'b -> string

    // Write a test function that calls the above function with an empty list, a list of one element, and a list of
    // multiple elements. Display an appropriate message to the terminal after each function call. 
    let list1 = []              // val list1 : 'a list
    let list2 = ["cat"]         // val list2 : string list = ["cat"]
    let list3 = [1;2;3;4;5]     // val list3 : int list = [1; 2; 3; 4; 5]
    let test () =
        printfn "%A" (Last_Element list1 list1) // "Invalid input" val it : unit = ()
        printfn "%A" (Last_Element list2 list2) // "The last element of ["cat"] is "cat"" val it : unit = ()
        printfn "%A" (Last_Element list3 list3) // "The last element of [1; 2; 3; 4; 5] is 5" val it : unit = ()
        ()

/////////////////////////// 3/11/2019 ///////////////////////////////////////////////////////////////////////////////////
module problem10_set2 =
// write an F# program to evaluate arithmetic expressions written in the language given by the following
// context-free grammar:
//                         E -> n | -E | E + E | E - E | E * E | E / E | (E)

    type 'a option = None |Some of 'a

    type Exp = Num of int
              |Neg of Exp
              |Sum of Exp*Exp
              |Diff of Exp*Exp
              |Prod of Exp*Exp
              |Quot of Exp*Exp

    let rec evaluate= function
        |Num n->Some n
        |Neg e -> match evaluate e with
                  |Some x -> Some (-x)
                  | _ -> None
        |Sum(e1,e2) -> match evaluate e1, evaluate e2 with
                       |Some x, Some y -> Some (x+y)
                       |_,None->None
                       |None, _-> None
        |Diff (e1,e2)->match evaluate e1, evaluate e2 with
                       |Some x, Some y -> Some (x-y)
                       |_,None->None
                       |None,_-> None
        |Prod (e1,e2)->match evaluate e1, evaluate e2 with
                       |Some x, Some y -> Some (x*y)
                       |_,None->None
                       |None, _-> None
        |Quot (e1,e2)->if(evaluate e2= (Some 0)) then None
                                                 else match evaluate e1, evaluate e2 with
                                                      |Some x, Some y -> Some (x/y)
                                                      |_,None->None
                                                      |None,_-> None
    // val evaluate : _arg1:Exp -> int option

    let test ()=
        let result1 = evaluate (Prod(Num 3, Diff(Num 5, Num 1))) 
        printfn "%A" result1 // some 12
        ()
    // val test : unit -> unit


///////////////////////////////// 3/13/2019 //////////////////////////////////////////////////////////////////
module problem12_set2 =
// Binary Search Tree
// Write a function that searches for a value in a binary search tree and then removes that node. 

    type Tree<'a> = 
        | Leaf
        | Branch of 'a * Tree<'a> * Tree<'a>
        

    let rec findSmallest = function
        | Leaf -> failwith "Empty trees!!!...there si no value"
        | Branch (x, Leaf, _) -> x
        | Branch(_, t1, _) -> findSmallest t1
    // val findSmallest : _arg1:Tree<'a> -> 'a

    let rec remove x = function
        | Leaf -> Leaf
        | Branch (y, Leaf, Leaf) -> if x= y then Leaf else Branch(y, Leaf, Leaf)
        | Branch (y, Leaf, t2) -> if x = y then t2 
                                  elif x > y 
                                  then Branch(y, Leaf, remove x t2)
                                  else Branch(y, Leaf, t2)
        | Branch (y, t1, Leaf) -> if x = y then t1 
                                  elif x < y 
                                  then Branch(y, remove x t1, Leaf)
                                  else Branch (y, t1, Leaf)
        | Branch (y, t1, t2)   -> if x = y then let smallest = findSmallest t2
                                                (Branch(smallest, t1, remove smallest t2))
                                  elif x < y then Branch(y, remove x t1, t2)
                                  else Branch(y, t1, remove x t2)
    //  val remove : x:'a -> _arg1:Tree<'a> -> Tree<'a> when 'a : comparison

    let rec insert newValue (tree : Tree<'a>) =
        match tree with
        | Leaf -> Branch (newValue, Leaf, Leaf)
        | Branch (value, left, right) when newValue < value ->
                                let left = insert newValue left
                                Branch (value, left, right)
        | Branch (value, left, right) when newValue > value -> let right = insert newValue right
                                                               Branch (value, left, right)
        | _ -> tree
    //  val insert : newValue:'a -> tree:Tree<'a> -> Tree<'a> when 'a : comparison

    let rec inorder (tree: Tree<'a>) =
        match tree with
        | Leaf -> []
        | Branch(value, left, right) -> value :: inorder (left) @ inorder(right)
    // val inorder : tree:Tree<'a> -> 'a list
    
    let test() =
        let result1 = Leaf |> insert 3 |> insert 5 |> insert 7 |> insert 3 |> insert 10 |> remove 10 |> remove 1 |> inorder
        printfn "%A" result1 // [3;5;7]
        ()
    
///////////////////////////////// 3/17/2019 ///////////////////////////////////////////////////////////
module TailRecursiveExample1 =
// function that return a list of number starting from 0 in the patter of skip n, choosen n, skip n.
    let indicesForStepTail start blockSize maxSize =
    // start : firts value of the list, blockSize: block to skip, maxSize: max size list
        let rec indicesForStepInternal istart accumList =
            match istart with
            | i when i > maxSize -> accumList |> List.rev
            | _ -> 
                let acc = 
                    [for j in ((min (istart + blockSize) maxSize) - 1) .. -1 .. istart -> j] 
                    @ accumList
                indicesForStepInternal (istart + 2 * blockSize) acc
        indicesForStepInternal start []
    //val indicesForStepTail : start:int -> blockSize:int -> maxSize:int -> int list

    let test () =
       
        let list2 = indicesForStepTail 5 4 20  // sol: [5;6;7;8;13;14;15;16]
        printfn "%A " list2
        ()


///////////////////////////////// 3/18/2019 //////////////////////////////////////////////////////////////////
// testing quiz 2 solutions!!!... //
module quiz2 =
    // problem 5 function find_largest with tail recursive
    // my solution
    //let rec find_largest list = function
    //    | [] -> None
    //    | [x] -> Some x
    //    | x::x1::xs->if x > x1 then find_largest x::xs
    //                           else find_largest x1::xs

    //professor solution
    let find_largest2 list =
        let rec loop max = function
            | [] -> None
            | [x] -> if x < max then Some max
                                else Some x
            | x::xs -> if x < max then loop max xs
                                  else loop x xs 
        loop 0 list
    //val find_largest2 : list:int list -> int option

    let test() =
        let list = [1;5;7;2;4;8;0;3]
       // let largest = find_largest list
        let largest2 = find_largest2 list
       // printfn "The largest number in %A is: %A " list largest
        printfn "The largest number in %A is: %A " list largest2
        ()

///////////////////////////////////////// 3/19/2019 ///////////////////////////////////////////////////
module examples1 = 
// Tryfind for a list .... using tail recursive
    let tryFind predicate xs =
       let rec loop = function
           | x::xs -> if predicate x then Some x else loop xs
           | _ -> None
       loop xs
    // val tryFind : predicate:('a -> bool) -> xs:'a list -> 'a option

// Quicksort needs a function to split a list around a "pivot" element
    let rec split1 pivot = function
       | []    -> ([],[])
       | x::xs -> let (left,right) = split1 pivot xs
                  if x < pivot then (x::left, right)
                               else (left, x::right)
    // val split1 : pivot:'a -> _arg1:'a list -> 'a list * 'a list when 'a : comparison
                            
    let rec qsort = function
    | []    -> []
    | [x]   -> [x]
    | x::xs -> let (left, right) = split1 x xs    // x is the pivot
               qsort left @ x :: qsort right
    // val qsort : _arg1:'a list -> 'a list when 'a : comparison

    let test()=
        let list1 = [6;2;9;1;4;6;1]
        let list2 = split1 4 list1
        let list3 = qsort list1
        printfn "%A split in %A and with qsort %A" list1 list2 list3
        ()

//////////////////////////////// coding 3 /////////////////////////////////////////////////////////////////////

// from exam 2 question 13. about BST.... take a BST as input and places all the even numbers into a list
module exam2_question13 =
    // Type for nodes in an BST
    type Tree = Lf | Br of int * Tree * Tree

    let rec evens = function // val evens : _arg1:Tree -> int list
        | Lf -> []
        | Br(n,left,rigth) -> if n % 2 = 0
                              then evens left @ n :: evens rigth
                              else evens left @ evens rigth
        | _ -> failwith "Error!..."

    let test () = //  val test : unit -> unit
        let bst1 = Br(20, 
                   Br(10, Lf, Lf), 
                   Br(30,Br (5, Lf, Lf), 
                   Br(40, Lf, Lf)))
        let listevens = evens bst1
        printfn "This are the evens numbers from the BST: %A" listevens
        ()

module problem1_Set3 =
    // Create a discriminated union that can be represent a linked list of integers
    type 'a LinkedList = Node of 'a * 'a LinkedList | EndNode  

    let rec List_linked = function //val List_linked : _arg1:'a list -> 'a LinkedList
        | [] -> EndNode
        | x::xs -> Node(x, List_linked xs)
    //val List_linked : _arg1:'a list -> 'a LinkedList

    let test()=
        let list1 = [2;4;5;6;3;8]  //val list1 : int list = [2; 4; 5; 6; 3; 8]
        let list1_linked = List_linked list1
        //val list1_linked : int LinkedList = Node (2,Node (4,Node (5,Node (6,Node (3,Node (8,EndNode))))))
        printfn "Original List: %A and linked list: %A" list1 list1_linked
        ()
    
module problem8_set3 =
    open System.Numerics
// problem 8
// Create a tail-recursive function that has a big integer as input and calculates 2I raised to that power.
// Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.

    let raise2I x =
        let rec loop x acc =
            match x with
            |_ when x = 0I -> acc
            |n -> loop (x - 1I) (acc * 2I)
        loop x 1I
    // val raise2I : x:System.Numerics.BigInteger -> System.Numerics.BigInteger

    let test()=
        let x0 = 0I
        let x1 = 1I
        let x2 = 2I
        let x3 = 4I
        let x4 = 16I
        let x5 = 256I
        let x6 = 1024I
       // let x7 = 32768I
       // let x8 = 65536I
        let x00 = raise2I x0
        let x11 = raise2I x1
        let x22 = raise2I x2
        let x33 = raise2I x3
        let x44 = raise2I x4
        let x55 = raise2I x5
        let x66 = raise2I x6
       // let x77 = raise2I x7
       // let x88 = raise2I x8
        printfn "The powers of %A is: %A" x0 x00
        printfn "The powers of %A is: %A" x1 x11
        printfn "The powers of %A is: %A" x2 x22
        printfn "The powers of %A is: %A" x3 x33
        printfn "The powers of %A is: %A" x4 x44
        printfn "The powers of %A is: %A" x5 x55
        printfn "The powers of %A is: %A" x6 x66
       // printfn "The powers of %A is: %A" x7 x77
       // printfn "The powers of %A is: %A" x8 x88
        ()

////////////////////////////// 4/5/2019 ///////////////////////////////////////////////////////////////////////////

module problem6_set3 =
// Generate an infinite sequence for the alternating series of 1/(2**n)
    let infiniteSequence = Seq.initInfinite( fun index -> let num = index + 1
                                                          (-1.0 ** float(num))/(2.0 ** float(num))
    ) //val infiniteSequence : seq<float>

// Display the 5th through 15th numbers in the series. The numbers should display as
// the floating point version of the fractions
    let numberSeries = infiniteSequence.GetEnumerator()   //val numberSeries : System.Collections.Generic.IEnumerator<float>
    
    let test()=
        for i = 0 to 3 do
            numberSeries.MoveNext()|> ignore
        printfn "infinite sequence!..."
        for i = 0 to 10 do
            numberSeries.MoveNext()|> ignore
            if i < 10 then printfn "%A, " numberSeries.Current
            else printfn "%A" numberSeries.Current
        ()

// Repeat the exercise using an infinite stream
    type 'a stream =
    | Cons of 'a * (unit -> 'a stream)

    let rec infiniteStream n = Cons ((-1.0 ** n) / (2.0 ** n), fun () -> infiniteStream (n + 1.0))
    
    let test1 () =
        let alterSeries = infiniteStream 1.0
        printfn "Infinite Stream!... "
        let rec take n (Cons(x,xs)) = if n = 0 then [] else x::take(n-1)(xs())
        let rec drop n (Cons(x,xs)) = if n = 0 then Cons(x,xs) else drop(n-1)(xs())
        let drop4 = drop 4 alterSeries
        let fiveToFifteen = take 11 drop4
        printfn "%A" fiveToFifteen
        ()

///////////////////////////////// 4/7/2019 -> 4/9/2019 /////////////////////////////////////////////////////

module PalindromeParser =
    open System.Runtime.InteropServices
    open System.Runtime.InteropServices

// Problem3 set 3
    // Write a CFG to recognize palindromes over the alphabet {a, b, |}, with the bar in the middle.

    // X -> aXa | bXb | |

    // Write a parse function that accepts a string and generates tokens for the language
    type PALINDROME = A | B | BAR | EOF | ERROR of string

    //  type PALINDROME =
    //  | A
    //  | B
    //  | BAR
    //  | EOF
    //  | ERROR 
    
   
    let rec parsePalindrome position (tmp: string) acc =
        if (position < 0) then acc
        else
            let letter = tmp.[position]
            match letter with
            | 'a' -> parsePalindrome (position - 1) tmp (A::acc)
            | 'b' -> parsePalindrome (position - 1) tmp (B::acc)
            | '|' -> parsePalindrome (position - 1) tmp (BAR::acc)
            |  v  -> parsePalindrome (position - 1) tmp (ERROR(sprintf "%A is not in our language." v)::acc)
    // val firstpass : position:int -> tmp:string -> acc:PALINDROME list -> PALINDROME list
    
    let parse (tmp: string) = parsePalindrome (tmp.Length - 1) tmp [EOF]
    // val parse : tmp:string -> PALINDROME list

    let test()=
        printf "Enter a palindrome word: "
        let input = Console.ReadLine()    // read the word from console
        let word1 = input|> string         
        let ispalindrome = parse word1
        printfn "%A" ispalindrome
        ()

    // Write a syntax checker that verifies if a list of tokens represents a palindrome
    // Extend the syntax checker so it generates an abstract syntax tree and displays it, for valid palindromes.
    type PalindromeTree = Lf of PALINDROME | Tree of PalindromeTree * PalindromeTree * PalindromeTree


    let eat t = function
        | x::xs -> if x = t then xs else failwith (sprintf "Need %A, but got %A" t x)
        | [] -> failwith " Empty! program end!!!...."
    // val eat : t:'a -> _arg1:'a list -> 'a list when 'a : equality

    let rec SyntaxChecker = function
        | [] -> failwith " Empty! program end!!!...."
        | x::xs -> match x with
                   | A   -> let (tree, tmp) = xs |> SyntaxChecker
                            let tmp = tmp |> eat A
                            Tree (Lf A, tree, Lf A), tmp
                   | B   -> let (tree, tmp) = xs |> SyntaxChecker
                            let tmp = tmp |> eat B
                            Tree (Lf B, tree, Lf B), tmp
                   | BAR -> (Lf BAR), xs
                   | v   -> (Lf (ERROR(sprintf "ERROR... invalid input!"))), x::xs
    
    let makeTree t =
        let (tree, tmp) = SyntaxChecker t
        tree
              
    let test1 ()=
        printf "Enter a palindrome word: "
        let input = Console.ReadLine()    // read the word from console
        let word1 = input|> string       
        let ispalindrome = parse word1
        let showTree = SyntaxChecker ispalindrome
        printfn "\n%A is Palindrome... !!!" ispalindrome
        printfn "\nThe Tree is :\n  %A" showTree
        ()

//////////////////////////////////////////////////////////////////////////////////////////////////
module problem3_set3_version2 =
    type palindrome = A | B | BAR 

    let parsePalindrome (s:string) =
        [for c in s do 
            match c with 
            |'a' ->yield A
            |'b' ->yield B
            |'|' ->yield BAR
            | v  ->failwith "ERROR... invalid input!"
        ]
    // val parsePalindrome : s:string -> PALINDROME list

    let eat l palindrome = 
        match l with
        |x::xs ->if x=palindrome then xs else failwith(sprintf "cannot eat %A from %A" palindrome (x::xs))
        |x     ->failwith(sprintf "cannot eat %A from %A" palindrome x)
    // val eat : l:'a list -> palindrome:'a -> 'a list when 'a : equality

    let rec Palindrome = function
        |[]      ->failwith "Error!"
        |A::xs   ->eat (Palindrome xs) A
        |B::xs   ->eat (Palindrome xs) B
        |BAR::xs ->xs
    // val Palindrome : _arg1:palindrome list -> palindrome list

    type Ptree=Lf of palindrome|Pal of palindrome*Ptree*palindrome

   
/////////////////////////////////////// 4/9/2019 /////////////////////////////////////////////////
module problem1_Set3_anotherOne =
// Problem 1 set 3 another way
// Building a simple tree. Create a discriminated union that can represent a linked list of integers.
// Write a function that converts a list into a linked list of nodes.
    type node = {Value:int; Next: List} 
    and List = Node of node | Empty

    let rec ListToLinkedList = function
        | [] -> Empty
        | [x] -> Node {Value = x; Next = Empty}
        | x::xs -> Node {Value = x; Next = (ListToLinkedList xs)}

    let test()=
        let list1 = [2;4;5;6;3;8]  //val list1 : int list = [2; 4; 5; 6; 3; 8]
        let list1_linked = ListToLinkedList list1
        
        printfn "Original List: %A and linked list: %A" list1 list1_linked
        ()

/////////////////////////////////////////////////////////////////////////////////////////////////
module problem5_Set3 = 
// Write a tail-recursive F# function interleave(xs,ys) that interleaves two lists.
// Assume that the two lists have the same length.

    let rec interleaveMain acc (xs, ys) =
        match (xs, ys) with
        |[],[] -> acc
        |[],_ -> failwith " different lengths!!!"
        |_,[] -> failwith " different lengths!!!"
        |x::xs, y::ys -> interleaveMain (x::y::acc) (xs, ys)
    //val interleaveMain : acc:'a list -> xs:'a list * ys:'a list -> 'a list

    let interleave (xs,ys) = interleaveMain [] (xs, ys)
    //val interleave : xs:'a list * ys:'a list -> 'a list

    let testA()=
        let list1 = [1..2..19999]
        let list2 = [2..2..20000]
        //let list1 = [1;3;5]
        //let list2 = [2;4;6]
        let interleaveList = interleave (list1,list2)
        printfn "%A and %A -> interleave result: %A" list1 list2 interleaveList
        ()
    // note: there is no measure when testing with list 1 and list 2 
    // the interleave of problem set1 take 7s

    let testB()=
        let list1 = [1..2..199999]
        let list2 = [2..2..200000]
        //let list1 = [1;3;5]
        //let list2 = [2;4;6]
        let interleaveList = interleave (list1,list2)
        
        printfn "%A and %A -> interleave result: %A" list1 list2 interleaveList
        ()
    // note: there is no measure when testing with list 1 and list 2 
    // the interleave of problem set1 crashes

////////////////////////////////////// 4/10/2019 /////////////////////////////////////////////////////////////
module problem7_Set3 =
// Generate an infinite stream for the the natural numbers greater than zero that are divisible by each element
// in a list of four elements. Use four, nested calls of filter on the infinite stream of natural numbers starting
// at one
// For example the output for the list [2;3;21;10]: 210, 420, 630, 840, 1050, ...

    // Greater Common Divisor ...... this is from problem set 1
    let rec gcd = function
        | (a,0) -> a
        | (a,b) -> gcd (b,a%b)

    // Least Common Multiple
    let lcm (a,b) = if a = 0 || b = 0 then 0 else (a*b)/(gcd(a,b))

    let rec inner_LCM acc = function
        | [] -> acc
        | [x] -> inner_LCM (lcm(acc, x)) []
        | x::xs -> inner_LCM (lcm(acc, x)) xs

    let getting_LCM xs = inner_LCM 1 xs

    
    let test()=
        let list = [2;3;21;10]
        let LCM = getting_LCM list  // getting only the first value.... in this case 210!
        printfn "The infinite stream for %A is : %A" list LCM
        ()


    // stream
    type 'a stream = Cons of 'a * (unit -> 'a stream)

    let rec upfrom n = Cons(n, fun () -> upfrom (n+1) )
    // val upfrom : n:int -> int stream

    let nats = upfrom 1
    //val nats : int stream = Cons (1,<fun:upfrom@1152>)

    let rec filter testf (Cons(x, xsf)) =
        if testf x then Cons(x, fun () -> filter testf (xsf()))
        else filter testf (xsf())
    //val filter : testf:('a -> bool) -> 'a stream -> 'a stream

    let rec take n (Cons(x, xsf)) =
        if n = 0 then []
        else x::take (n-1) (xsf ())
    //val take : n:int -> 'a stream -> 'a list

    let rec drop n (Cons (x , xsf)) =
        if n = 0 then Cons (x, xsf)
        else drop (n-1) (xsf())
    //val drop : n:int -> 'a stream -> 'a stream

    let rec fetch n (Cons (x , xsf)) =
        if n = 0 then x
        else fetch (n-1) (xsf())
    //val fetch : n:int -> 'a stream -> 'a

    let test1()=
        let list = [2;3;21;10]
        let LCM = getting_LCM list
        let result1 = take 4 (drop 21 (filter (fun n -> (n % LCM) = 0) nats))
        printfn "%A" result1
        ()


    //sequence
    let test2() =
        let list = [2;3;21;10]
        let LCM = getting_LCM list

        let seqProb7 = 
            Seq.initInfinite (fun index ->
            let n = index + 1
            n * LCM)
    
        let result1 = Seq.take 4 (Seq.skip 21 seqProb7)
        printfn "%A" result1
        ()

//////////////////////////////////////// 4/12/2019 /////////////////////////////////////////////////////
module problem11_Set3 = 
// Write a non-recursive fibonacci function using imperative F#.Compare the timing with a tail-recursive fibonacci.
    //imperative
    let impFibonacci fibonacciNumber =
     // using ref.... for mutable reference
        let current = ref 1   // val current : int ref = {contents = 1;}
        let before =  ref 1   // val before : int ref = {contents = 1;}
        let after = ref 0     // val after : int ref = {contents = 0;} 

        if fibonacciNumber = 0 then 0 else
        if fibonacciNumber = 1 || fibonacciNumber = 2 then 1 else
        for i in 1..fibonacciNumber - 2 do
            after := !current + !before      // !  go to the value and := change the value
            before := !current 
            current := !after
        !after
    //val impFibonacci : fibonacciNumber:int -> int

    let test()=
    //testing imperative fibonacci
        printf "Enter a number: "
        let input = Console.ReadLine()    // read the word from console
        let number = input|> int       
        let result1 = impFibonacci number
        printfn "The imperative fibonacci of %A is: %A " number result1
        ()

    // Tail-recursive from professor lecture.
    let tailFibonacci = function
        | n when n = 0I -> 0I
        | n when n = 1I -> 1I
        | n -> let rec loop m2 m1 m =
                    match (m2,m1,m) with
                    |(_,_,m) when m = 0I -> m2
                    |(_,_,m) when m = 1I -> m1
                    |_ -> loop m1 (m1 + m2)(m-1I)
               loop 0I 1I n
    //val tailFibonacci :_arg1:System.Numerics.BigInteger -> System.Numerics.BigInteger
               
    let test1()=
    //testing tail recusive fibonacci
        printf "Enter a number: "
        let input = Console.ReadLine()    // read the word from console
        let number = input|> int|> bigint  
        let result2 = tailFibonacci number
        printfn "The tail-recursive fibonacci of %A is %A: " number result2
        ()

    
//////////////////////////////////////////////////////////////////////////////////////////////////////
module program12_Set3 =
// Using imperative F#, create a record type for a student. The record will have a function that returns
// the student's GPA, a function that adds credit hours and a function that adds grade points. Initialize an
// instance of the record with appropriate functions and values. Use the instance to add grade points and credits
// several times, and display the GPA.

    type student = {mutable CreditHours: float; mutable GradePoints: float} with
        member this.getGPA = 
            if this.CreditHours < 1.0 then 0.0 else (this.GradePoints / this.CreditHours)
            member this.addHours value = this.CreditHours <- this.CreditHours + value
            member this.addPoints value = this.GradePoints <- this.GradePoints + value
        

    
    
    let test()= 
        let gp (grade:float) hours = grade * hours        //  val gp : grade:float -> hours:float -> float
        let Bob = {CreditHours = 0.0; GradePoints = 0.0}; //  val Bob : student = {CreditHours = 9.0; GradePoints = 15.0;}
        Bob.getGPA|>ignore
        Bob.addHours 3.0
        Bob.addHours 3.0
        Bob.addHours 3.0
        Bob.addPoints (gp 3.0 3.0) // grade B
        Bob.addPoints (gp 4.0 3.0) // grade A
        Bob.addPoints (gp 2.0 3.0) // grade C
        Bob.getGPA|>ignore

        printfn "credit hours %A"  Bob.CreditHours|>ignore
        printfn "gradePoints %A"   Bob.GradePoints|>ignore
        ()

module problem12_set3_A =
    type student = {getGPA: unit -> float; getCredits: unit -> float; addCreditsHours: float -> unit; 
                    addGradePoints: float -> unit}

    let test()=
        let newStudent =
            let credits = ref 0.0
            let gpa     = ref 0.0
            {
                getGPA = fun() -> !gpa
                getCredits = fun() -> !credits
                addCreditsHours = fun n -> credits := !credits + n
                addGradePoints = fun n -> gpa := !gpa + n
            }

        newStudent.addCreditsHours 12.0
        newStudent.addCreditsHours 6.0
        newStudent.addGradePoints 3.5
        printfn "Student GPA: %A" (newStudent.getGPA())
        printfn "Student Credits: %A" (newStudent.getCredits())
        ()

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
module problem13_Set3 =
//Using imperative F#, create a tuple for an integer stack, including push, pop, top and isEmpty functions.
//Use the stack to implement factorial. Use a loop to push all the values from 1 through the parameter,
//then use another loop to pop the values and calculate factorial. Compare the timing with a tail-recursive factorial.

    type stack = {mutable Node: option<int> * option<stack>} with
        member this.isEmpty =
            match this.Node with
            |None, None -> true
            |n , _ -> false
        member this.push value =
            let tempV, tempS = this.Node
            match this.Node with
            |None, None -> this.Node <- ((Some value), None)
            |n, _ -> this.Node <- ((Some value), Some {Node = tempV, tempS})
        member this.pop =
            match this.Node with
            |None, None -> failwith "Stack is empty!"
            |Some n, None ->
                let temp = n
                this.Node <- (None, None)
                temp
            | v, s -> 
                let temp = v.Value
                this.Node <- s.Value.Node
                temp
        member this.top =
            match this.Node with
            |None, None -> failwith "Stack is empty!"
            | value, _ -> value.Value

    let stack = {Node = None, None}
    for i in 1 .. 10 do
        stack.push i

    let result1 = stack.Node
    let result2 = stack.top
    
    // factorial
    let fact n =
        let myStack = {Node  = None, None}
        let mutable result = 1;
    
        for i in 1 .. n do
            myStack.push i
    
        while myStack.isEmpty = false do
            result <- result * myStack.pop
        result

    
    let test() =
        printf "Factorial Calculator....Enter the number: "
        let input = Console.ReadLine()    // read the word from console
        let number = input|> int  
        let impfactorial = fact number
        printfn "the Factorial of %A is: %A" number impfactorial
        ()

    //factorial tail recursive
    //acc always start in 1 because !0 = 1 
    let rec tailfactorial acc n =
        match n with
        | 0 -> acc
        | n -> tailfactorial (acc * n) (n - 1)

    //acc always start in 1 because !0 = 1 
    let factorialNumber n = tailfactorial 1 n

    let test1() =
        printf "Tail Factorial Calculator....Enter the number: "
        let input = Console.ReadLine()    // read the word from console
        let number = input|> int  
        let impfactorial = factorialNumber number
        printfn "the Factorial of %A is: %A" number impfactorial
        () 

    let rec doesfacs (f:int->int) (acc:int list) n =
        match n with
        |0 -> acc
        |n -> doesfacs f ((f n)::acc) (n - 1)
    //val doesfacs : f:(int -> int) -> acc:int list -> n:int -> int list

    let test2() =
        let res1 = doesfacs factorialNumber [] 100
       // let res2 = doesfacs fact [] 100
        printfn "%A" res1
      //  printfn "%A" res2
        ()

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
module problem18_set3 =
//problem 18 set3
//Declare type measures for seconds, microseconds, milliseconds, and nanoseconds.
    [<Measure>] type seconds
    [<Measure>] type microseconds
    [<Measure>] type milliseconds
    [<Measure>] type nanoseconds

//Declare constants for the number of seconds in each of the other types.
//Create functions that convert seconds to each of the other types. What is the principal type of each function?
    let secondsInMicroseconds = 0.000001<seconds/microseconds> // val secondsInMicroseconds : float<seconds/microseconds> = 1e-06
    let secondsToMicroseconds (s : float<seconds>) = s / secondsInMicroseconds
    // val secondsToMicroseconds : s:float<seconds> -> float<microseconds>

    let secondsInMilliseconds = 0.001<seconds/milliseconds>   //val secondsInMilliseconds : float<seconds/milliseconds> = 0.001
    let secondsToMilliseconds (s : float<seconds>) = s / secondsInMilliseconds
    //val secondsToMilliseconds : s:float<seconds> -> float<milliseconds>

    let secondsInNanoseconds = 0.000000001<seconds/nanoseconds> //val secondsInNanoseconds : float<seconds/nanoseconds> = 1e-09
    let secondsToNanoseconds (s : float<seconds>) = s / secondsInNanoseconds
    //val secondsToNanoseconds : s:float<seconds> -> float<nanoseconds>

//Create functions that convert each of the other types to seconds. What is the principal type of each function?
    let microsecondsToSeconds (micro : float<microseconds>) = micro * secondsInMicroseconds
    let millisecondsToSeconds (milli : float<milliseconds>) = milli * secondsInMilliseconds
    let nanosecondsToSeconds (nano : float<nanoseconds>) = nano * secondsInNanoseconds

    let test()=
    //Convert 5000 milliseconds to seconds and then to microseconds.
        let x = millisecondsToSeconds 5000.0<milliseconds>   // val x : float<seconds> = 5.0 
        let y = secondsToMicroseconds x                      // val y : float<microseconds> = 5000000.0

    //Convert 0.00000009 seconds to microseconds and to nanoseconds.
        let a = secondsToMicroseconds 0.00000009<seconds>    // val a : float<microseconds> = 0.09
        let b = secondsToNanoseconds 0.00000009<seconds>     // val b : float<nanoseconds> = 90.0
    //printing result                                                                                                                              
        printfn "When we convert 5000 milliseconds to seconds the result is: %A " x                                                                               
        printfn "When we convert 5000 seconds to microseconds the result is: %A " y
        printfn "When we convert 0.00000009 seconds to microseconds the result is: %A " a
        printfn "When we convert 0.00000009 seconds to microseconds the result is: %A " b
        ()                                                                                          
        

//////////////////////////////// 4/16/2019 ///////////////////////////////////////////////////////////
/////////////////////////////// FROM QUIZ 3 //////////////////////////////////////////////////////////
//.... tail recursive function that sum square of even number in a list
module quiz3_SumSQR=

    let rec sumSQR acc  = function
        | []    -> acc
        | [x]   -> if (x % 2) = 0 then acc + x*x else acc
        | x::xs -> if (x % 2) = 0 then sumSQR (acc + x*x)  xs else sumSQR acc xs
    

    //***** professor solution *******//    
    let evenADD list  =
        let rec tmp acc = function
            | [] -> acc 
            | x::xs -> if x % 2 = 0 then tmp(acc + x*x) xs
                                    else tmp acc xs
        tmp 0 list
           


    let testsumSQR()=
        printf "Enter the number of element in your list: "
        let input = Console.ReadLine()    // read from console
        let number = input|> int
        
        // This is how I get my list from console.
        printfn "---------------------------------------------"
        printfn "Enter the elements:"
        let rec getList (cnt:int, acc: int list): int list =
            if cnt = 0 then
                acc
            else
                let n = Console.ReadLine() |> int
                getList (cnt-1, n::acc)
        
        let list = getList (number,[]) // calling my list with the number of element in a list given.
        let sumList = sumSQR 0 list    // 
        printfn "\nThe sum of the square of all even number in the list %A is: %A" list sumList
                      
        ()

    let testQR1()=
        printf "Enter the number of element in your list: "
        let input = Console.ReadLine()    // read from console
        let number = input|> int
        
        // This is how I get my list from console.
        printfn "---------------------------------------------"
        printfn "Enter the elements:"
        let rec getList (cnt:int, acc: int list): int list =
            if cnt = 0 then
                acc
            else
                let n = Console.ReadLine() |> int
                getList (cnt-1, n::acc)
        
        let list = getList (number,[]) // calling my list with the number of element in a list given.
        let sumList = sumSQR 0 list    // 
        printfn "\nThe sum of the square of all even number in the list %A is: %A" list sumList
                      
        ()
    
    //testing professor soutio

// From Quiz # 3
// Generate an infinite sequence for the alternating series (float) of 1/(3**n) 
module infiniteSecuenceQuiz3 =

    let test()=
        let infiniteSequence = Seq.initInfinite( fun index -> let num = index + 1
                                                              1.0/(3.0 * float(num)))
         //val infiniteSequence : seq<float>

        let numberSeries = infiniteSequence.GetEnumerator()
        // val numberSeries : System.Collections.Generic.IEnumerator<float>

        printf "Enter the number of sequences: "
        let input = Console.ReadLine()    // read from console
        let number = input|> int

        printfn "This is the infinite sequence!..."
        for i = 1 to number do
            numberSeries.MoveNext()|> ignore
            if i < number then printfn " %A, " numberSeries.Current
            else printfn " %A" numberSeries.Current
        ()

 ///////// solution from professor
 ///////// almost same that already I have.
    let test1()=
        let s = Seq.initInfinite(fun index -> let n = float(index + 1)
                                              1.0/ (3.0 * n))

        let numberSeries = s.GetEnumerator()
        printf "Enter the number of sequences: "
        let input = Console.ReadLine()    // read from console
        let number = input|> int

        printfn "This is the infinite sequence!..."
        for i = 1 to number do
            numberSeries.MoveNext()|> ignore
            if i < number then printfn " %A, " numberSeries.Current
            else printfn " %A" numberSeries.Current
        ()
/////////// solution from professor using stream
    type 'a stream = Cons of 'a * (unit -> 'a stream)

    let rec thirds n = 
        Cons (1.0/(3.0 * n), fun () -> thirds (n + 1.0))

    
////////////////////////////////////////////////////////////////////////////////////////////////
module BTsecuence =
    // Yield the values of a binary tree in a sequence.
    type Tree<'a> =
       | Tree of 'a * Tree<'a> * Tree<'a>
       | Leaf of 'a

    // inorder : Tree<'a> -> seq<'a>
    let rec inorder tree =
        seq {
          match tree with
              | Tree(x, left, right) ->
                   yield! inorder left
                   yield x
                   yield! inorder right
              | Leaf x -> yield x
        }
    let test()=
        let mytree = Tree(8, Tree(2, Leaf(1), Leaf(3)), Leaf(9))
        let seq1 = inorder mytree
        printfn "%A" seq1
        ()

///////////////////////////////////////////////////////////////////////////////////////////////////