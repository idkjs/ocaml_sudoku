module hints.hiddenQuad

// Hidden Pair means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open core.sudoku
open hidden

let hiddenQuadsPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi 
            (fun i symbol1 -> 
            Seq.mapi 
                (fun j symbol2 -> 
                Seq.mapi (fun k symbol3 -> 
                    Seq.mapi (fun l symbol4 -> 
                        let symbols = set [ symbol1; symbol2; symbol3; symbol4 ]

                        findHidden symbols houseCandidates candidateCells house) (Seq.skip (i + j + k + 3) salphabet)) 
                    (Seq.skip (i + j + 2) salphabet)) (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let hiddenQuadFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenQuadsPerHouse alphabet candidateLookup houseCells) houses
