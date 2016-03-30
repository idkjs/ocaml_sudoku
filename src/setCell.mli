open Sudoku
open Puzzlemap

val apply : puzzleMap -> value -> current -> current

val try' : cell -> digit -> cellCandidates -> value option

val description : puzzleMap -> value -> Hint.description

val step : puzzleMap -> value -> solution -> solution
