open Sudoku
open Puzzlemap

val description : puzzleMap -> candidate -> Hint.description

val step : puzzleMap -> candidate -> solution -> solution
