module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.setCell
open core.sudoku
open core.puzzlemap

open hints

let fullHousePerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : MapCellCandidates) (primaryHouse : House) : Set<HintDescription2> =
    let primaryHouseCells = puzzleHouseCells primaryHouse

    let candidateCells = Set.map (fun cell -> ((candidateLookup.Item cell), cell)) primaryHouseCells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells
    
    let hhs2 = 
        if hhs.Count = 1 then 
            let h = first hhs
            let cell = snd h
            let candidate = first (fst h)

            let setCellValue = makeSetCellDigit cell candidate

            [ { HintDescription.primaryHouses = set [ primaryHouse ]
                secondaryHouses = set []
                candidateReductions = set []
                setCellValueAction = Some setCellValue
                pointers = set [] } ]
        else []

    hhs2
    |> Set.ofList
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)
