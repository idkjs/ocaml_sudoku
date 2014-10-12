module core.clearCandidate

open sudoku

val clearCandidateApply : ClearCellCandidateAction -> (Cell -> CellContents) -> Cell -> CellContents

type ClearCellCandidateError = 
    | AlreadySet of Symbol
    | NotACandidate

val clearCandidateTry : Cell
     -> Candidate -> (Cell -> CellContents) -> Either<ClearCellCandidateAction, ClearCellCandidateError>
