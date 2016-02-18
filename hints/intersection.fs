module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

let intersectionsPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (candidateLookup : MapCellCandidates) 
    (puzzleHouseCells : House -> Set<Cell>) (primaryHouse : House) (secondaryHouseLookups : Map<Cell, Set<House>>) : Set<HintDescription2> = 
    let primaryHouseCells = puzzleHouseCells primaryHouse
    
    let primaryHouseCandidates = 
        Set.map (fun cell -> candidateLookup.Item cell) primaryHouseCells
        |> Set.unionMany
        |> Set.toList
    
    let uniqueSecondaryForCandidate candidate : Set<HintDescription> = 
        let pointerCells = 
            Set.filter (fun cell -> 
                let candidates = candidateLookup.Item cell
                Set.contains candidate candidates) primaryHouseCells
        
        let pointers = 
            Set.map (fun cell -> 
                { CandidateReduction.cell = cell
                  candidates = set [ candidate ] }) pointerCells
        
        let hintsPerSecondaryHouse (secondaryHouses : Set<House>) : HintDescription option = 
            if Set.count pointerCells > 1 && Set.count secondaryHouses = 1 then 
                let secondaryHouse = first secondaryHouses
                let secondaryHouseCells = puzzleHouseCells secondaryHouse
                let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductionCells = 
                    Set.filter (fun cell -> 
                        let candidates = candidateLookup.Item cell
                        Set.contains candidate candidates) otherHouseCells
                
                let candidateReductions = 
                    Set.map (fun cell -> 
                        { CandidateReduction.cell = cell
                          candidates = set [ candidate ] }) candidateReductionCells
                
                if Set.count candidateReductions > 0 then 
                    Some { HintDescription.primaryHouses = set [ primaryHouse ]
                           secondaryHouses = set [ secondaryHouse ]
                           candidateReductions = candidateReductions
                           setCellValueAction = None
                           pointers = pointers }
                else None
            else None
        
        pointerCells
        |> Set.map (fun cell -> 
                        let secondaryHouses : Set<House> = secondaryHouseLookups.Item cell
                        hintsPerSecondaryHouse secondaryHouses)
        |> Set.filter Option.isSome
        |> Set.map Option.get
    
    primaryHouseCandidates
    |> Set.ofList
    |> Set.map uniqueSecondaryForCandidate
    |> Set.unionMany
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)

let pointingPairsPerBox (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : MapCellCandidates) (primaryHouse : House) : Set<HintDescription2> =
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [HRow cell.row; HColumn cell.col ]
            |> Set.ofList
        allCells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse allCells cellHouseCells candidateLookup puzzleHouseCells primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : MapCellCandidates) (puzzleCellBox : Cell -> Box) (primaryHouse : House) : Set<HintDescription2> = 
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [HBox(puzzleCellBox cell) ]
            |> Set.ofList
        allCells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse allCells cellHouseCells candidateLookup puzzleHouseCells primaryHouse secondaryHouseLookups
