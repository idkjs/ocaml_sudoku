module console.command

open core.sudoku

val setCellCommand : string -> Candidate list -> (Cell -> CellContents) -> Cell list -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> SetCellValue option

val candidateClearCommand : string -> Candidate list -> (Cell -> CellContents) -> Cell list -> ClearCandidate option
