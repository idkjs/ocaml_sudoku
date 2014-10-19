module console.command

open core.sudoku

val parseValue : Digit list -> string -> Digit option

val setCellCommand : string
     -> Digit list
     -> (Cell -> CellContents)
     -> Cell list -> (Cell -> Set<Cell>) -> (Cell -> Set<Digit>) -> Value option

val candidateClearCommand : string
     -> Digit list -> (Cell -> CellContents) -> Cell list -> Candidate option
