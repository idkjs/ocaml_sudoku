module core.clearCandidate

open System

open core.sudoku

let clearCandidateApply (clearCandidate : ClearCandidate) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (entryLookup : Cell -> CellContents) (cell : Cell) -> 
        let entry = entryLookup cell

        match entry with
        | ASymbol _ -> entry
        | ACandidates candidates -> 
            if clearCandidate.cell = cell then 
                ACandidates (Set.remove clearCandidate.candidate candidates)
            else entry

let clearCandidateTry (candidate : Candidate) (entryLookup : Cell -> CellContents) cell = 
    match entryLookup cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates candidates -> 
        if Set.contains candidate candidates then 
            Some { ClearCandidate.cell = cell
                   candidate = candidate }
        else
            Console.WriteLine("Cell {0} does not have candidate {1}", cell, candidate)
            None
