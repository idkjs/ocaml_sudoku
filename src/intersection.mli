open Sudoku
open Puzzlemap
open hints

val pointingPairs : puzzleMap -> cellCandidates -> hintDescription list

val boxLineReductions : puzzleMap -> cellCandidates -> hintDescription list
