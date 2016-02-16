module console.command

open core.sudoku
open core.puzzlemap

val parseValue : Digit list -> string -> Digit option

val setCellCommand : string
     -> Digit list
     -> Current
     -> Cell list -> MapCellHouseCells -> MapCellCandidates -> Value option

val candidateClearCommand : string
     -> Digit list -> Current -> Cell list -> Candidate option
