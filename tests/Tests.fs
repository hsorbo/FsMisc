module Tests
open System
open FsMisc
open Xunit
open Swensen.Unquote

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
        printfn "Thinking"
        let count = FsMisc.SortedSeqDiff.seqDiff id (seq { 0 .. 10000 }) (seq { 0 .. 10000 }) |> Seq.length
        printfn "%A per ms" ((decimal count) / (decimal s.ElapsedMilliseconds))
