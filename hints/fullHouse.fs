module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.setCell
open core.sudoku
open core.puzzlemap

open hints

let fullHousePerHouse (allCells : Set<Cell>) (cellHouseCells : CellHouseCells) (puzzleHouseCells : HouseCells) 
    (candidateLookup : CellCandidates) (primaryHouse : House) : Set<HintDescription2> =

    let candidateCells =
        primaryHouse
        |> puzzleHouseCells.Get
        |> Set.map (fun cell -> ((candidateLookup.Get cell), cell))

    let hhs =
        candidateCells
        |> Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) 
    
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
            |> Set.ofList
        else Set.empty

    hhs2
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)
