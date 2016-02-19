module console.command

open core.sudoku
open core.puzzlemap

val parseValue : Digit list -> string -> Digit option

val setCellCommand : string
     -> Digit list
     -> Current
     -> Set<Cell> -> CellHouseCells -> CellCandidates -> Value option

val candidateClearCommand : string
     -> Digit list -> Current -> Set<Cell> -> Candidate option
