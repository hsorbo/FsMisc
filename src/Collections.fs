namespace FsMisc

module SortedSeqDiff =
    type SeqDiff<'a> =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a

    let private moveNext (e: System.Collections.Generic.IEnumerator<_>) =
        match e.MoveNext() with
        | true -> e.Current |> Some
        | false -> None

    let private diff chooser = function
    | (Some l, Some r) when chooser l < chooser r -> Left(l)
    | (Some l, Some r) when chooser l > chooser r -> Right(r)
    | (Some l, Some r)                            -> Both(l,r)
    | (Some l, None)                              -> Left(l)
    | (None, Some r)                              -> Right(r)
    | (None, None)                                -> invalidOp "Both none"

    let seqDiff chooser (left : seq<_>) (right : seq<_>) = seq {
        use lft = left.GetEnumerator()
        use rgh = right.GetEnumerator()
        let mutable workLeft = moveNext lft
        let mutable workRight = moveNext rgh
        while (Option.isNone workLeft && Option.isNone workRight) |> not do
            let dir = diff chooser (workLeft,workRight)
            match dir with
            | Left _  -> workLeft  <- moveNext lft
            | Right _ -> workRight <- moveNext rgh
            | Both _  -> workLeft  <- moveNext lft
                         workRight <- moveNext rgh
            yield dir
    }

// let private tryHeadAndTail s = s |> Seq.tryHead |> Option.map (fun h -> h, s |> Seq.skip 1)

// let rec seqDiff chooser left right = seq {
//     let (l,r) = (left |> tryHeadAndTail, right |> tryHeadAndTail)
//     let diff =
//         match (l,r) with
//         | (Some (lh, _), Some (rh, _)) when chooser lh < chooser rh -> Some <| Left(lh)
//         | (Some (lh, _), Some (rh, _)) when chooser lh > chooser rh -> Some <| Right(rh)
//         | (Some (lh, _), Some (rh, _))                              -> Some <| Both(lh,rh)
//         | (Some (lh, _), None)                                      -> Some <| Left(lh)
//         | (None, Some  (rh, _))                                     -> Some <| Right(rh)
//         | (None, None)                                              -> None
    
//     match diff with
//     | Some x -> yield x
//     | None -> ()

//     match (l,r) with
//     | (Some (lh, lt), Some (rh, _)) when chooser lh < chooser rh -> yield! seqDiff chooser lt right
//     | (Some (lh, _), Some (rh, rt)) when chooser lh > chooser rh -> yield! seqDiff chooser left rt
//     | (Some (_, lt), Some (_, rt))                               -> yield! seqDiff chooser lt rt
//     | (Some (_, lt), None)                                       -> yield! seqDiff chooser lt right
//     | (None, Some  (_, rt))                                      -> yield! seqDiff chooser left rt
//     | (None, None)                                               -> ()
// }

//     let private tryHeadAndTail s = s |> Seq.tryHead |> Option.map (fun h -> h, s |> Seq.skip 1)

//     let rec seqDiff chooser (left : seq<_>) (right : seq<_>) = seq {
//         let (l,r) = (left |> tryHeadAndTail, right |> tryHeadAndTail)
//         match (l,r) with
//         | (Some (lh, lt), Some (rh, _)) when chooser lh < chooser rh 
//             -> yield Left(lh)
//                yield! seqDiff chooser lt right
//         | (Some (lh, _), Some (rh, rt)) when chooser lh > chooser rh 
//             -> yield Right(rh)
//                yield! seqDiff chooser left rt
//         | (Some (lh, lt), Some (rh, rt))
//             -> yield Both(lh,rh)
//                yield! seqDiff chooser lt rt
//         | (Some (lh, lt), None)   
//             -> yield Left(lh)
//                yield! seqDiff chooser lt right
//         | (None, Some  (rh, rt))
//             -> yield Right(rh)
//                yield! seqDiff chooser left rt
//         | (None, None) -> ()
//     }