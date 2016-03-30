open Sudoku
open Puzzlemap

val find : p:puzzleMap -> current:current -> candidateReduction list
val apply : p:puzzleMap -> candidateReductions:candidateReduction list -> current:current -> current
val description : p:puzzleMap -> candidateReductions:candidateReduction list -> Hint.description
val step : p:puzzleMap -> solution:solution -> candidateReductions:candidateReduction list -> solution
val findAndApply : p:puzzleMap -> solution:solution -> solution
