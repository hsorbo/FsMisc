namespace FsMisc

module SortedSeqDiff =
    type SeqDiff<'a> =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a

    let private tryHeadAndTail s = s |> Seq.tryHead |> Option.map (fun h -> h, s |> Seq.skip 1)

    let rec seqDiff chooser (left : seq<_>) (right : seq<_>) = seq {
        let (l,r) = (left |> tryHeadAndTail, right |> tryHeadAndTail)
        match (l,r) with
        | (Some (lh, lt), Some (rh, _)) when chooser lh < chooser rh 
            -> yield Left(lh)
               yield! seqDiff chooser lt right
        | (Some (lh, _), Some (rh, rt)) when chooser lh > chooser rh 
            -> yield Right(rh)
               yield! seqDiff chooser left rt
        | (Some (lh, lt), Some (rh, rt))
            -> yield Both(lh,rh)
               yield! seqDiff chooser lt rt
        | (Some (lh, lt), None)   
            -> yield Left(lh)
               yield! seqDiff chooser lt right
        | (None, Some  (rh, rt))
            -> yield Right(rh)
               yield! seqDiff chooser left rt
        | (None, None) -> ()
    }

    