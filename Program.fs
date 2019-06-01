module Program

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn " %A\n" argv

    ////////////////////////// coding # 1 //////////////////////////////////////////////////////////

    //printfn " %d\n" (3 + 4)
    //printfn " Succ of 13 is %d\n" (succ 13)
    //printfn " Pred of 13 is %d\n" (code.pred 13)

    //code.Problems1.test()                     // succ and pred of a number.
    //code.Problems2.test()                     // List.... head of the list.
    //code.Problems3.test()                     // testing fibonacci.
    //code.square_number.test()                 // square value of a integer number.
    //code.List_prod.test()                     // multiply together all the number is a list.
    //code.Problems16.addFraction()             // additon of two fractions.
    //code.Problems16.multFraction()            // multiplication of two fractions.
    //code.Problems16.reverseExample()          // reverse List problem 17 problem set 1.
    //code.Problem18_interleave.test()          // interleave two list.
    //code.Problem18_interleave.printSum()      // Sum element of two list and put the result in a new list.
    //code.Problem19_CUT.test_gencut()          // Cut a list in two pieces where n is the size of first piece 
    //code.Problem19_CUT.print_CUT()            // cut a list in two equal spaces.
    //code.Problem20_shuffle.print_shuffle()    // shuffle a List

    ////////////////////////// coding # 2 ////////////////////////////////////////////////////////

    //code.Problem1_Set2.test()                 // Testing problem 1 set 2
    //code.Problem2_Set2.test()                 // Testing problem 2 set 2 
    //code.Examples_Testing.test()              // Testing some Examples
    //code.Examples_Testing1.test()             // Testing parse_check example from class
    //code.prob2_set2.test()                    // Another way in problem 2 set 2
    //code.Sort_Example.test()                  // Example of sorting
    //code.problem6_set2.test1()                // Testing problem 6 set 2
    //code.problem3_set2.test()                 // Testing problem 3 set 2
    //code.problem5_set2.test()                 // Testing problem 5 set 2
    //code.problem9_set2.test()                 // Testing problem 9 set 2   
    //code.problem10_set2.test()                // Testing problem 10 set 2
    //code.problem12_set2.test()                // Testing problem 12 set 2
    //code.TailRecursiveExample1.test()
    //code.quiz2.test()
    //code.examples1.test()
    //code.exam2_question13.test()

    ////////////////////////// coding # 3 ////////////////////////////////////////////////////////

    //code.problem1_Set3.test()                   // function that converts a list into a linked list of nodes.
    //code.problem1_Set3_anotherOne.test()        // function that converts a list into a linked list of nodes. 
    //code.PalindromeParser.test()                // Problem3 set 3 parse function that accepts a string and generates tokens for the language
    //code.PalindromeParser.test1()               // Problem3 set 3 syntax checker that verifies if a list of tokens represents a palindrome
    //code.problem5_Set3.testA()                  // tail-recursive F# function interleave(xs,ys) that interleaves two lists.
    //code.problem5_Set3.testB()                  // interleave of problem set1
    //code.problem6_set3.test()                   // infinite sequence for the alternating series of 1/(2**n)
    //code.problem6_set3.test1()                  // infinite stream for the alternating series of 1/(2**n)
    //code.problem7_Set3.test()                   // infinite stream for the the natural numbers greater than zero that are divisible by each element
                                                  // in a list of four elements.

    //code.problem7_Set3.test1()                  // stream same problem 7
    //code.problem7_Set3.test2()                  // secuence same problem 7
    //code.problem8_set3.test()                   // tail-recursive calculates 2I raised to that power
    //code.problem11_Set3.test()                  // non-recursive fibonacci function using imperative
    //code.problem11_Set3.test1()                 // Tail-recursive from professor lecture.
    //code.program12_Set3.test()                  // imperative F#, create a record type for a student... record will have a function, display the GPA
    //code.problem12_set3_A.test()                // Testig problem 12 GPA and Credits.
    //code.problem13_Set3.test()                  // imperative
    //code.problem13_Set3.test1()                 // tail-recursive factorial.
    //code.problem13_Set3.test2()                 // more testing on problem 13
    //code.problem18_set3.test()                  // Testing, declaring Measures
    

    ////////// Problem 16 Interp /////////////////////////////////////////////////////////////////////
    //Interp.testInterp()                           // Testing the PCF interpreter

    ////////// Problem 17 "W algorithm for all the terms in PCF" ////////////////////////////////////
    //type_example.test()

    //////////////// quiz 3 solutions /////////////////////
    //code.quiz3_SumSQR.testsumSQR()
    //code.quiz3_SumSQR.testQR1()
    //code.infiniteSecuenceQuiz3.test()
    //code.infiniteSecuenceQuiz3.test1()
    //code.infiniteSecuenceQuiz3.test2()


    //////////////////////////////////////////////////////
    //testing somestuff
    code.BTsecuence.test()

    printfn "\n"
    printfn "Press Enter to continue ..."
    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
