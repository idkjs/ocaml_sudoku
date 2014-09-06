module hints.nakedPair

open core.sudoku
open naked

let nakedPairPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 1 && Set.count candidates <= 2) candidateCells

    let hs =
        Seq.mapi (fun i candidatesCell1 -> 
            Seq.mapi (fun j candidatesCell2 -> 
                let candidatesCellAll = set [candidatesCell1; candidatesCell2 ]
                findNaked candidatesCellAll candidateCells house) (Seq.skip (i + 1) hht)) hht

    let hhs = 
        hs
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList


let nakedPairFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedPairPerHouse candidateLookup houseCells) houses
