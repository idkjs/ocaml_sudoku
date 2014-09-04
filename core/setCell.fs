module core.setCell

open System

open core.sudoku

let setCellValueModelEffect (setCellValue : SetCellValue) (cellHouseCells : Cell -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) : Set<CandidateReduction> = 
    let houseCells = cellHouseCells setCellValue.cell
    let otherHouseCells = Set.remove setCellValue.cell houseCells
    
    let candidateReductions = 
        Set.filter (fun c -> 
            let cs = candidateLookup c
            Set.contains setCellValue.candidate cs) otherHouseCells
    
    let candidateReductionCells = 
        Set.map (fun c -> 
            { CandidateReduction.cell = c
              symbols = set [ setCellValue.candidate ] }) candidateReductions
    
    candidateReductionCells

let setCellCandidateReductions (setCellValue : SetCellValue) (cellHouseCells : Cell -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) : Set<CandidateReduction> = 
    let candidateReductionCells = setCellValueModelEffect setCellValue cellHouseCells candidateLookup

    let ccs = candidateLookup setCellValue.cell
    let ccs2 = Set.remove setCellValue.candidate ccs
    
    let crs = 
        { CandidateReduction.cell = setCellValue.cell
          symbols = ccs2 }
    
    let crs3 = Set.add crs candidateReductionCells

    crs3


let setCellApply (setCellValue : SetCellValue) (cellHouseCells : Cell -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate> = 
    let candidateReductions = setCellValueModelEffect setCellValue cellHouseCells candidateLookup

    fun (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let entry = entryLookup cell
        
        let cr = 
            { CandidateReduction.cell = cell
              symbols = set [ setCellValue.candidate ] }
        match entry with
        | Given _ | Set _ -> entry
        | Candidates candidates -> 
            let candidateToSymbol (Candidate s : Candidate) = Symbol s

            if setCellValue.cell = cell then Set(candidateToSymbol setCellValue.candidate)
            else 
                let f s = 
                    let hs = candidates s
                    if candidateReductions.Contains cr && s = setCellValue.candidate then Removed
                    else hs
                Candidates f

let setCellTry (candidate : Candidate) (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) cell = 
    match entryLookup cell with
    | Given s -> 
        Console.WriteLine("Cell {0} has been given value {1}", cell, s)
        None
    | Set s -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, s)
        None
    | Candidates _ -> 
        Some { SetCellValue.cell = cell
               candidate = candidate }


