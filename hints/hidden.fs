module hints.hidden

open core.sudoku
open core.puzzlemap
open core.hints

let findHidden (count : int) (p : puzzleMap) (candidateLookup : cellCandidates) (candidateSubset : Set<digit>) (primaryHouse : house) = 

    let primaryHouseCells = p.houseCells.Get primaryHouse

    let pairs = 
        primaryHouseCells
        |> Set.map (fun cell -> 
            let candidates = candidateLookup.Get cell
            
            let pointer = 
                { candidateReduction.cell = cell
                  candidates = Set.intersect candidates candidateSubset }
            
            let crs = 
                if Set.count pointer.candidates > 0 then Set.difference candidates candidateSubset
                else set []
            
            let candidateReduction = 
                { candidateReduction.cell = cell
                  candidates = crs }
            
            (pointer, candidateReduction))
        |> Set.toList

    let pointers, candidateReductions = List.unzip pairs

    let nonEmptyPointers =
        pointers
        |> List.filter (fun cr -> Set.count cr.candidates > 0) 

    let nonEmptyCandidateReductions =
        candidateReductions
        |> List.filter (fun cr -> Set.count cr.candidates > 0) 

    if List.length nonEmptyPointers = count && List.length nonEmptyCandidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = List.head nonEmptyPointers
                let cell = h.cell
                let candidate = first candidateSubset

                let setCellValue = makeSetCellDigit cell candidate

                Some setCellValue
            else None

        Some { hintDescription.primaryHouses = set [ primaryHouse ]
               secondaryHouses = set []
               candidateReductions = Set.ofList nonEmptyCandidateReductions
               setCellValueAction = setCellValue
               pointers = Set.ofList nonEmptyPointers
               focus = set [] }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (candidateLookup : cellCandidates) (house : house) : Set<hintDescription> = 
    let cells = p.houseCells.Get house

    let houseCandidates =
        cells
        |> Set.map candidateLookup.Get
        |> Set.unionMany

    setSubsets (Set.toList houseCandidates) count
    |> Set.ofList
    |> Set.map
        (fun candidateSubset -> 
            findHidden count p candidateLookup (Set.ofList candidateSubset) house)
    |> Set.filter Option.isSome
    |> Set.map Option.get

let hiddenN (i : int) (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    p.houses
    |> Set.map (hiddenNPerHouse i p candidateLookup)
    |> Set.unionMany
