module core.clearCandidate

open sudoku

val clearCandidateApply : ClearCellCandidate -> (Cell -> CellContents) -> Cell -> CellContents

val clearCandidateTry : Cell -> Candidate -> (Cell -> CellContents) -> ClearCellCandidate option
