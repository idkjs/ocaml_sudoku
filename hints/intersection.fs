module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

let intersectionsPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (candidateLookup : MapCellCandidates) 
    (puzzleHouseCells : MapHouseCells) (primaryHouse : House) (secondaryHouseLookups : Map<Cell, Set<House>>) : Set<HintDescription2> = 
    let primaryHouseCells = puzzleHouseCells.Item primaryHouse
    
    let primaryHouseCandidates : Set<Digit> = 
        primaryHouseCells
        |> Set.map (fun cell -> candidateLookup.Item cell) 
        |> Set.unionMany
    
    let uniqueSecondaryForCandidate (candidate : Digit) : Set<HintDescription> = 
        let pointerCells = 
            primaryHouseCells
            |> Set.filter (fun cell -> 
                let candidates = candidateLookup.Item cell
                Set.contains candidate candidates) 
        
        let pointers : Set<CandidateReduction> = 
            pointerCells
            |> Set.map (fun cell -> 
                { CandidateReduction.cell = cell
                  candidates = set [ candidate ] }) 
        
        let hintsPerSecondaryHouse (secondaryHouses : Set<House>) : HintDescription option = 
            if Set.count pointerCells > 1 && Set.count secondaryHouses = 1 then 
                let secondaryHouse = first secondaryHouses
                let secondaryHouseCells = puzzleHouseCells.Item secondaryHouse
                let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductionCells =
                    otherHouseCells
                    |> Set.filter (fun cell -> 
                        let candidates = candidateLookup.Item cell
                        Set.contains candidate candidates) 
                
                let candidateReductions = 
                    candidateReductionCells
                    |> Set.map (fun cell -> 
                        { CandidateReduction.cell = cell
                          candidates = set [ candidate ] }) 
                
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
    |> Set.map uniqueSecondaryForCandidate
    |> Set.unionMany
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)

let pointingPairsPerBox (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : MapHouseCells) 
    (candidateLookup : MapCellCandidates) (primaryHouse : House) : Set<HintDescription2> =
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [HRow cell.row; HColumn cell.col ]
            |> Set.ofList
        allCells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse allCells cellHouseCells candidateLookup puzzleHouseCells primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : MapHouseCells) 
    (candidateLookup : MapCellCandidates) (puzzleCellBox : MapCellBox) (primaryHouse : House) : Set<HintDescription2> = 
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [ puzzleCellBox.Item cell |> HBox ]
            |> Set.ofList
        allCells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse allCells cellHouseCells candidateLookup puzzleHouseCells primaryHouse secondaryHouseLookups
