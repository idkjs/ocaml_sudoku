module hints.intersection

open core.puzzlemap
open core.sudoku
open hints

let intersectionsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (primaryHouse : House) (secondaryHouseLookups : (Cell -> House) list) = 

    let primaryHouseCells = houseCells primaryHouse
    
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
                  symbols = set [ candidate ] }) pointerCells
        
        let hintsPerSecondaryHouse secondaryHouses = 
            if Set.count pointerCells > 1 && Set.count secondaryHouses = 1 then 
                let secondaryHouse = first secondaryHouses
                let secondaryHouseCells = houseCells secondaryHouse
                let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductionCells = 
                    Set.filter (fun cell -> 
                        let candidates = candidateLookup cell
                        Set.contains candidate candidates) otherHouseCells
                
                let candidateReductions = 
                    Set.map (fun cell -> 
                        { CandidateReduction.cell = cell
                          symbols = set [ candidate ] }) candidateReductionCells
                
                if Set.count candidateReductions > 0 then 
                    Some { HintDescription.primaryHouses = set [ primaryHouse ]
                           secondaryHouses = set [ secondaryHouse ]
                           candidateReductions = candidateReductions
                           setCellValue = None
                           pointers = pointers }
                else None
            else None
        
        List.choose (fun secondaryHouseLookup -> 
            let secondaryHouses = Set.map secondaryHouseLookup pointerCells
            hintsPerSecondaryHouse secondaryHouses) secondaryHouseLookups
    
    List.collect uniqueSecondaryForCandidate primaryHouseCandidates

let pointingPairsPerBox (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (primaryHouse : House) = 
    intersectionsPerHouse candidateLookup houseCells primaryHouse [ (fun cell -> Row cell.row)
                                                                    (fun cell -> Column cell.col) ]

let boxLineReductionsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (boxWidth : int<width>) (boxHeight : int<height>) (primaryHouse : House) = 
    intersectionsPerHouse candidateLookup houseCells primaryHouse [ (fun cell -> Box(cellBox boxWidth boxHeight cell)) ]
