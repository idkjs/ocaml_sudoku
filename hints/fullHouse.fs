module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.setCell
open core.sudoku
open hints

let fullHousePerHouse (cellHouseCells : Cell -> Set<Cell>) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) (primaryHouse : House) = 
    let primaryHouseCells = puzzleHouseCells primaryHouse

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) primaryHouseCells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells
    
    let hhs2 = 
        if hhs.Count = 1 then 
            let h = first hhs
            let cell = snd h
            let candidate = first (fst h)

            let setCellValue = makeSetCellSymbol cell candidate

            [ { HintDescription.primaryHouses = set [ primaryHouse ]
                secondaryHouses = set []
                candidateReductions = set []
                setCellValueAction = Some setCellValue
                pointers = set [] } ]
        else []

    List.map (mhas cellHouseCells puzzleHouseCells) hhs2
