module core.setCell

open sudoku
open puzzlemap
open hints

val setCellDigitApply : puzzleMap -> value -> current -> current

val setCellDigitTry : cell -> digit -> cellCandidates -> value option

val setCellHintDescription : puzzleMap -> value -> hintDescription

val setCellStep : puzzleMap -> value -> solution -> solution
