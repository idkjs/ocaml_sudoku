module hints.pointingPair

open core.sudoku
open hints

let pointingPairsPerBox (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (box : House) = 

    let boxCells = houseCells box

    let boxCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) boxCells
    
    let boxCandidates = 
        Set.map fst boxCandidateCells
        |> Set.unionMany
        |> Set.toList
    
    let makeHint candidate hht house = 
        let houseCells = houseCells house

        let otherHouseCells = Set.difference houseCells boxCells

        let candidateReductionsCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) otherHouseCells
        let hht2 = 
            Set.filter (fun (candidates, _) -> Set.contains candidate candidates) candidateReductionsCandidateCells
        
        let candidateReductions = 
            Set.map (fun (_, cell) -> 
                { CandidateReduction.cell = cell
                  symbols = set [ candidate ] }) hht2
        
        let pointers = 
            Set.map (fun (_, cell) -> 
                { CandidateReduction.cell = cell
                  symbols = set [ candidate ] }) hht
        
        if Set.count candidateReductions > 0 then 
            Some { HintDescription.house = Some box
                   candidateReductions = candidateReductions
                   setCellValue = None
                   pointers = pointers }
        else None
    
    let uniqueRowForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) boxCandidateCells
        if Set.count hht > 1 then 
            let rows = Set.map (fun (_, cell) -> cell.row) hht
            if Set.count rows = 1 then makeHint candidate hht (Row(first rows))
            else None
        else None
    
    let uniqueColForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) boxCandidateCells
        if Set.count hht > 1 then 
            let cols = Set.map (fun (_, cell) -> cell.col) hht
            if Set.count cols = 1 then makeHint candidate hht (Column(first cols))
            else None
        else None
    
    let rows = List.choose uniqueRowForCandidate boxCandidates

    let cols = List.choose uniqueColForCandidate boxCandidates

    List.concat [ rows; cols ]


let pointingPairFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    let boxes = 
        List.filter (fun house -> 
            match house with
            | Box _ -> true
            | _ -> false) houses
    List.collect (pointingPairsPerBox candidateLookup houseCells) boxes
