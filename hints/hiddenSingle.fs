module hints.hiddenSingle

// Hidden Single means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open core.sudoku
open hidden

let hiddenSinglesPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi (fun i symbol1 -> 
            let symbols = set [ symbol1 ]

            findHidden symbols houseCandidates candidateCells house) salphabet
    
    let hhs = hs

    Seq.choose id hhs |> Seq.toList

let hiddenSingleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenSinglesPerHouse alphabet candidateLookup houseCells) houses
