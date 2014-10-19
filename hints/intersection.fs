module hints.intersection

open core.sudoku
open hints

let intersectionsPerHouse (cellHouseCells : Cell -> Set<Cell>) (candidateLookup : Cell -> Set<Digit>) 
    (puzzleHouseCells : House -> Set<Cell>) (primaryHouse : House) (secondaryHouseLookups : (Cell -> House) list) = 
    let primaryHouseCells = puzzleHouseCells primaryHouse
    
    let primaryHouseCandidates = 
        Set.map candidateLookup primaryHouseCells
        |> Set.unionMany
        |> Set.toList
    
    let uniqueSecondaryForCandidate candidate = 
        let pointerCells = 
            Set.filter (fun cell -> 
                let candidates = candidateLookup cell
                Set.contains candidate candidates) primaryHouseCells
        
        let pointers = 
            Set.map (fun cell -> 
                { CandidateReduction.cell = cell
                  candidates = set [ candidate ] }) pointerCells
        
        let hintsPerSecondaryHouse secondaryHouses = 
            if Set.count pointerCells > 1 && Set.count secondaryHouses = 1 then 
                let secondaryHouse = first secondaryHouses
                let secondaryHouseCells = puzzleHouseCells secondaryHouse
                let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductionCells = 
                    Set.filter (fun cell -> 
                        let candidates = candidateLookup cell
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
        
        List.choose (fun secondaryHouseLookup -> 
            let secondaryHouses = Set.map secondaryHouseLookup pointerCells
            hintsPerSecondaryHouse secondaryHouses) secondaryHouseLookups
    
    List.collect uniqueSecondaryForCandidate primaryHouseCandidates |> List.map (mhas cellHouseCells puzzleHouseCells)

let pointingPairsPerBox (cellHouseCells : Cell -> Set<Cell>) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Digit>) (primaryHouse : House) = 
    intersectionsPerHouse cellHouseCells candidateLookup puzzleHouseCells primaryHouse [ (fun cell -> Row cell.row)
                                                                                         (fun cell -> Column cell.col) ]

let boxLineReductionsPerHouse (cellHouseCells : Cell -> Set<Cell>) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Digit>) (puzzleCellBox : Cell -> Box) (primaryHouse : House) = 
    intersectionsPerHouse cellHouseCells candidateLookup puzzleHouseCells primaryHouse 
        [ (fun cell -> Box(puzzleCellBox cell)) ]
