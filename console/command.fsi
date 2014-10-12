module console.command

open core.sudoku

val setCellCommand : string
     -> Candidate list
     -> (Cell -> CellContents)
     -> Cell list -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> SetCellSymbolAction option

val candidateClearCommand : string
     -> Candidate list -> (Cell -> CellContents) -> Cell list -> ClearCellCandidateAction option
