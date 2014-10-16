module console.command

open core.sudoku

val setCellCommand : string
     -> Candidate list
     -> (Cell -> CellContents)
     -> Cell list -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> SetCellDigitAction option

val candidateClearCommand : string
     -> Candidate list -> (Cell -> CellContents) -> Cell list -> EliminateCandidateAction option
