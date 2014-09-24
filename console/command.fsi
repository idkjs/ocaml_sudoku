module console.command

open core.sudoku

val setCellCommand : string -> Candidate list -> (Cell -> CellContents) -> Cell list -> SetCellValue option

val candidateClearCommand : string -> Candidate list -> (Cell -> CellContents) -> Cell list -> ClearCandidate option
