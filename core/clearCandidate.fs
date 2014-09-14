module core.clearCandidate

open System

open core.sudoku

let clearCandidateApply (clearCandidate : ClearCandidate) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate> = 

    fun (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let entry = entryLookup cell

        match entry with
        | Given _ | Set _ -> entry
        | Candidates candidates -> 
            let candidateToSymbol (Candidate s : Candidate) = Symbol s

            if clearCandidate.cell = cell then 
                let clr candidate = 
                    if clearCandidate.candidate = candidate then Removed
                    else candidates candidate

                Candidates clr
            else entry

let clearCandidateTry (candidate : Candidate) (entryLookup : Cell -> AnnotatedSymbol<AnnotatedCandidate>) cell = 
    match entryLookup cell with
    | Given s -> 
        Console.WriteLine("Cell {0} has been given value {1}", cell, s)
        None
    | Set s -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, s)
        None
    | Candidates candidates -> 
        let c = candidates candidate
        match c with
        | Possible -> 
            Some { ClearCandidate.cell = cell
                   candidate = candidate }
        | Excluded | Removed -> 
            Console.WriteLine("Cell {0} does not have candidate {1}", cell, candidate)
            None
