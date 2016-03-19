open Sudoku
open Puzzlemap
open Hints

val setCellDigitApply : puzzleMap -> value -> current -> current

val setCellDigitTry : cell -> digit -> cellCandidates -> value option

val setCellHintDescription : puzzleMap -> value -> hintDescription

val setCellStep : puzzleMap -> value -> solution -> solution
