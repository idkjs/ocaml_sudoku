module hints.hiddenPair

// Hidden Pair means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open core.sudoku
open hidden

let hiddenPairsPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi (fun i symbol1 -> 
            Seq.mapi (fun j symbol2 -> 
                let symbols = set [ symbol1; symbol2 ]

                findHidden symbols houseCandidates candidateCells house) (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = Seq.concat hs

    Seq.choose id hhs |> Seq.toList

let hiddenPairFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenPairsPerHouse alphabet candidateLookup houseCells) houses
