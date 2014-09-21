module core.clearCandidate

open System

open core.sudoku

let clearCandidateApply (clearCandidate : ClearCandidate) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate> = 

    fun (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let entry = entryLookup cell

        match entry with
        | ASymbol _ -> entry
        | ACandidates candidates -> 
            if clearCandidate.cell = cell then 
                let clr candidate = 
                    if clearCandidate.candidate = candidate then Removed
                    else candidates candidate

                ACandidates clr
            else entry

let clearCandidateTry (candidate : Candidate) (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) cell = 
    match entryLookup cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates candidates -> 
        let c = candidates candidate
        match c with
        | Possible -> 
            Some { ClearCandidate.cell = cell
                   candidate = candidate }
        | Excluded | Removed -> 
            Console.WriteLine("Cell {0} does not have candidate {1}", cell, candidate)
            None
