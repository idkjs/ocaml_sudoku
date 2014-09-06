module hints.boxLineReduction

open core.puzzlemap
open core.sudoku
open hints

let boxLineReductionsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCellsMap : House -> Set<Cell>) 
    (boxWidth : int<width>) (boxHeight : int<height>) (house : House) = 
    let houseCells = houseCellsMap house

    let houseCandidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) houseCells
    
    let houseCandidates = 
        Set.map fst houseCandidateCells
        |> Set.unionMany
        |> Set.toList
    
    let makeHint candidate hht box = 
        let boxCells = houseCellsMap box

        let otherHouseCells = Set.difference boxCells houseCells

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
            Some { HintDescription.house = Some house
                   candidateReductions = candidateReductions
                   setCellValue = None
                   pointers = pointers }
        else None
    
    let uniqueBoxForCandidate candidate = 
        let hht = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells
        if Set.count hht > 1 then 
            let boxes = Set.map (fun (_, cell) -> cellBox boxWidth boxHeight cell) hht
            if Set.count boxes = 1 then makeHint candidate hht (Box(first boxes))
            else None
        else None
    
    List.choose uniqueBoxForCandidate houseCandidates

let boxLineReductionFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (houses : House list) (boxWidth : int<width>) (boxHeight : int<height>) = 
    let rows = 
        List.filter (fun house -> 
            match house with
            | Row _ -> true
            | _ -> false) houses
    
    let cols = 
        List.filter (fun house -> 
            match house with
            | Column _ -> true
            | _ -> false) houses
    
    let rowHints = List.collect (boxLineReductionsPerHouse candidateLookup houseCells boxWidth boxHeight) rows

    let colHints = List.collect (boxLineReductionsPerHouse candidateLookup houseCells boxWidth boxHeight) cols

    List.concat [ rowHints; colHints ]
