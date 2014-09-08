module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.sudoku
open hints

let fullHousePerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Count = 1 then 
        let h = first hhs

        [ { HintDescription.primaryHouses = set [ house ]
            secondaryHouses = set []
            candidateReductions = set []
            setCellValue = 
                Some { SetCellValue.cell = snd h
                       candidate = first (fst h) }
            pointers = set [] } ]
    else []

let fullHouseFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (fullHousePerHouse candidateLookup houseCells) houses
