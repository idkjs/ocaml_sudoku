module hints.hidden

open console

open core.setCell
open core.sudoku
open core.puzzlemap
open hints

let findHidden (cellHouseCells : MapCellHouseCells) (candidateLookup : MapCellCandidates) 
    (primaryHouseCells : Set<Cell>) (candidateSubset : Set<Digit>) (count : int) (primaryHouse : House) = 
    let pairs = 
        List.map (fun cell -> 
            let candidates = candidateLookup.Item cell
            
            let pointer = 
                { CandidateReduction.cell = cell
                  candidates = Set.intersect candidates candidateSubset }
            
            let crs = 
                if Set.count pointer.candidates > 0 then Set.difference candidates candidateSubset
                else set []
            
            let candidateReduction = 
                { CandidateReduction.cell = cell
                  candidates = crs }
            
            (pointer, candidateReduction)) (Set.toList primaryHouseCells)
    
    let pointers, candidateReductions = List.unzip pairs

    let nonEmptyPointers = List.filter (fun cr -> Set.count cr.candidates > 0) pointers

    let nonEmptyCandidateReductions = List.filter (fun cr -> Set.count cr.candidates > 0) candidateReductions

    if List.length nonEmptyPointers = count && List.length nonEmptyCandidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = List.head nonEmptyPointers
                let cell = h.cell
                let candidate = first candidateSubset

                let setCellValue = makeSetCellDigit cell candidate

                Some setCellValue
            else None

        Some { HintDescription.primaryHouses = set [ primaryHouse ]
               secondaryHouses = set []
               candidateReductions = Set.ofList nonEmptyCandidateReductions
               setCellValueAction = setCellValue
               pointers = Set.ofList nonEmptyPointers }
    else None

let hiddenNPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : MapHouseCells) 
    (candidateLookup : MapCellCandidates) (count : int) (house : House) : Set<HintDescription2> = 
    let cells = puzzleHouseCells.Item house

    let houseCandidates =
        cells
        |> Set.map (fun cell -> candidateLookup.Item cell)
        |> Set.unionMany

    let candidateSubsets = setSubsets (Set.toList houseCandidates) count
    let hs =
        candidateSubsets
        |> Set.ofList
        |> Set.map
            (fun candidateSubset -> 
            findHidden cellHouseCells candidateLookup cells (Set.ofList candidateSubset) count house)
        |> Set.filter Option.isSome
        |> Set.map Option.get

    hs
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)
