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
        | ASymbol _ -> entry
        | ACandidates candidates -> 
            let candidateToSymbol (Candidate s : Candidate) = ASymbol (Symbol s)

            if setCellValue.cell = cell then candidateToSymbol setCellValue.candidate
            else 
                let f s = 
                    let hs = candidates s
                    if candidateReductions.Contains cr && s = setCellValue.candidate then Removed
                    else hs
                ACandidates f

let setCellTry (candidate : Candidate) (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) cell = 
    match entryLookup cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates _ -> 
        Some { SetCellValue.cell = cell
               candidate = candidate }
