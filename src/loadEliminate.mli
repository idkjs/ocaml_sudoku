open Sudoku
open Puzzlemap

val find : puzzleMap -> current -> candidateReduction list
val apply : puzzleMap -> candidateReduction list -> current -> current
val description : puzzleMap -> candidateReduction list -> Hint.description
val step : puzzleMap -> solution -> candidateReduction list -> solution
val findAndApply : puzzleMap -> solution -> solution
