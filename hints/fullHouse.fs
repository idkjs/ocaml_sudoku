module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.setCell
open core.sudoku
open hints

let fullHousePerHouse (cellHouseCells : Cell -> Set<Cell>) (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (primaryHouse : House) = 

    let primaryHouseCells = houseCells primaryHouse

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) primaryHouseCells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Count = 1 then 
        let h = first hhs
        let cell = snd h
        let candidate = first (fst h)

        let setCellValue = makeSetCellValue cell candidate cellHouseCells candidateLookup

        [ { HintDescription.primaryHouses = set [ primaryHouse ]
            primaryHouseCells = primaryHouseCells
            secondaryHouses = set []
            secondaryHouseCells = set []
            candidateReductions = set []
            setCellValue = Some setCellValue
            pointers = set [] } ]
    else []
