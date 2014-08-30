module core.setCell

open System

open core.puzzlemap
open core.sudoku

let setCellApply (setCellValue : SetCellValue) (puzzleMaps : PuzzleMaps) (candidateLookup : Cell -> Set<Candidate>) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate> = 
    let candidateReductions = setCellValueModelEffect puzzleMaps setCellValue candidateLookup

    fun (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let entry = entryLookup cell
        
        let cr = 
            { CandidateReduction.cell = cell
              symbols = set [ setCellValue.candidate ] }
        match entry with
        | Given _ | Set _ -> entry
        | Candidates candidates -> 
            if setCellValue.cell = cell then Set(candidateToSymbol setCellValue.candidate)
            else 
                let f s = 
                    let hs = candidates s
                    if candidateReductions.Contains cr && s = setCellValue.candidate then Removed
                    else hs
                Candidates f

let setCellTry (puzzleMaps : PuzzleMaps) (candidate : Candidate) 
    (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) cell = 
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
