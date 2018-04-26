module Tests
open System
open FsMisc
open Xunit
open Swensen.Unquote
open FsCheck.Experimental

module SortedSeqDiffTests = 
    open SortedSeqDiff
    
    [<Fact>]
    let ``Generates correct diff`` () =
        let left = [1;2;4;6;8;10]
        let right = [1;3;4;5;6;11;12]
        let result = [Both (1,1); Left(2); Right(3); Both(4,4); Right(5); Both(6,6); Left(8); Left(10); Right(11); Right(12)]
        test <@ seqDiff id left right |> Seq.toList = result @>

    [<Fact>]
    let ``Can handle large sequences`` () =
        let s = System.Diagnostics.Stopwatch.StartNew()
        //printfn "Thinking"
        //let count = 
        FsMisc.SortedSeqDiff.seqDiff id (seq { 0 .. 1000000 }) (seq { 0 .. 1000000 }) |> Seq.length
        //printfn "%A per ms" ((decimal count) / (decimal s.ElapsedMilliseconds))

module NetRcTests =
    open NetRc
    let exampleSingle = @"
machine example.com
login daniel
password qwerty"
    
    let exampleTwo = @"
machine ayy
login loginA
password passA

machine bee
login loginB
password passB"

    let exampleDefault = @"default login anonymous password user@site"
    
    [<Fact>]
    let ``parses  exampleData1`` () =
        let expected = { Entry.ForMachine "example.com" with  Login = "daniel" |> Some; Password = "qwerty" |> Some }
        test <@ NetRc.parseString exampleSingle = [expected] @>
    
    [<Fact>]
    let ``parses  exampleTwo`` () =
        let a = { Entry.ForMachine "ayy" with  Login = "loginA" |> Some; Password = "passA" |> Some }
        let b = { Entry.ForMachine "bee" with  Login = "loginB" |> Some; Password = "passB" |> Some }
        test <@ NetRc.parseString exampleTwo = [a;b] @>
    
    [<Fact>]
    let ``parses  exampleDefault`` () =
        let expected = { Entry.Default with Login = "anonymous" |> Some; Password = "user@site" |> Some }
        test <@ NetRc.parseString exampleDefault = [expected] @>

