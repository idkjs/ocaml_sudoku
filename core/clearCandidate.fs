module core.clearCandidate

open System

open core.sudoku

let clearCandidateApply (clearCandidate : ClearCellCandidate) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell

        match cellContents with
        | ASymbol _ -> cellContents
        | ACandidates candidates -> 
            if clearCandidate.cell = cell then ACandidates(Set.remove clearCandidate.candidate candidates)
            else cellContents

let clearCandidateTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : ClearCellCandidate option = 
    match cellCellContents cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates candidates -> 
        if Set.contains candidate candidates then 
            Some { ClearCellCandidate.cell = cell
                   candidate = candidate }
        else 
            Console.WriteLine("Cell {0} does not have candidate {1}", cell, candidate)
            None
