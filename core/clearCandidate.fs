module core.clearCandidate

open sudoku

let clearCandidateApply (clearCandidate : ClearCellCandidateAction) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell

        match cellContents with
        | ASymbol _ -> cellContents
        | ACandidates candidates -> 
            if clearCandidate.cell = cell then ACandidates(Set.remove clearCandidate.candidate candidates)
            else cellContents

type ClearCellCandidateError = 
    | AlreadySet of Symbol
    | NotACandidate

let clearCandidateTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : Either<ClearCellCandidateAction, ClearCellCandidateError> = 
    match cellCellContents cell with
    | ASymbol symbol -> Right(AlreadySet symbol)
    | ACandidates candidates -> 
        if Set.contains candidate candidates then 
            Left { ClearCellCandidateAction.cell = cell
                   candidate = candidate }
        else Right NotACandidate
