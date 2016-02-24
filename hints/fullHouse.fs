module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open core.sudoku
open core.puzzlemap
open core.hints

let fullHousePerHouse (p : PuzzleMap) (candidateLookup : CellCandidates) (primaryHouse : House) : Set<HintDescription> =

    let candidateCells =
        primaryHouse
        |> p.houseCells.Get
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
                pointers = set []
                focus = set [] } ]
            |> Set.ofList
        else Set.empty

    hhs2

let fullHouses (p : PuzzleMap) (candidateLookup : CellCandidates) : Set<HintDescription> =
    p.houses
    |> Set.map (fullHousePerHouse p candidateLookup)
    |> Set.unionMany
