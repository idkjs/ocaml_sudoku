module hints.intersection

open core.puzzlemap
open core.sudoku
open hints

let makeHint candidate hht primaryHouse primaryHouseCells secondaryHouse secondaryHouseCells candidateLookup = 

    let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells

    let candidateReductionsCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) otherHouseCells
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) candidateReductionsCandidateCells
    
    let candidateReductions = 
        Set.map (fun (_, cell) -> 
            { CandidateReduction.cell = cell
              symbols = set [ candidate ] }) hht2
    
    let pointers = 
        Set.map (fun (_, cell) -> 
            { CandidateReduction.cell = cell
              symbols = set [ candidate ] }) hht
    
    if Set.count candidateReductions > 0 then 
        Some { HintDescription.primaryHouses = set [ primaryHouse ]
               secondaryHouses = set [ secondaryHouse ]
               candidateReductions = candidateReductions
               setCellValue = None
               pointers = pointers }
    else None

let pointingPairsPerBox (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (primaryHouse : House) = 

    let primaryHouseCells = houseCells primaryHouse

    let primaryHouseCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) primaryHouseCells
    
    let primaryHouseCandidates = 
        Set.map fst primaryHouseCandidateCells
        |> Set.unionMany
        |> Set.toList
    
    let uniqueRowForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) primaryHouseCandidateCells
        if Set.count hht > 1 then 
            let rows = Set.map (fun (_, cell) -> cell.row) hht
            if Set.count rows = 1 then 
                let secondaryHouse = Row(first rows)
                let secondaryHouseCells = houseCells secondaryHouse

                makeHint candidate hht primaryHouse primaryHouseCells secondaryHouse secondaryHouseCells candidateLookup
            else None
        else None
    
    let uniqueColForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) primaryHouseCandidateCells
        if Set.count hht > 1 then 
            let cols = Set.map (fun (_, cell) -> cell.col) hht
            if Set.count cols = 1 then 
                let secondaryHouse = Column(first cols)
                let secondaryHouseCells = houseCells secondaryHouse

                makeHint candidate hht primaryHouse primaryHouseCells secondaryHouse secondaryHouseCells candidateLookup
            else None
        else None
    
    let rows = List.choose uniqueRowForCandidate primaryHouseCandidates

    let cols = List.choose uniqueColForCandidate primaryHouseCandidates

    List.concat [ rows; cols ]


let pointingPairFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (boxes : Box list) = 
    List.collect (pointingPairsPerBox candidateLookup houseCells) (List.map Box boxes)


let boxLineReductionsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (boxWidth : int<width>) (boxHeight : int<height>) (primaryHouse : House) = 
    let primaryHouseCells = houseCells primaryHouse

    let primaryHouseCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) primaryHouseCells
    
    let primaryHouseCandidates = 
        Set.map fst primaryHouseCandidateCells
        |> Set.unionMany
        |> Set.toList
    
    let uniqueBoxForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) primaryHouseCandidateCells
        if Set.count hht > 1 then 
            let boxes = Set.map (fun (_, cell) -> cellBox boxWidth boxHeight cell) hht
            if Set.count boxes = 1 then 
                let secondaryHouse = Box(first boxes)
                let secondaryHouseCells = houseCells secondaryHouse

                makeHint candidate hht primaryHouse primaryHouseCells secondaryHouse secondaryHouseCells candidateLookup
            else None
        else None
    
    List.choose uniqueBoxForCandidate primaryHouseCandidates

let boxLineReductionFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (rows : Row list) 
    (cols : Column list) (boxWidth : int<width>) (boxHeight : int<height>) = 

    let rowHints = 
        List.collect (boxLineReductionsPerHouse candidateLookup houseCells boxWidth boxHeight) (List.map Row rows)

    let colHints = 
        List.collect (boxLineReductionsPerHouse candidateLookup houseCells boxWidth boxHeight) (List.map Column cols)

    List.concat [ rowHints; colHints ]

