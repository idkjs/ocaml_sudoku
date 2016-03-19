open Sudoku
open Puzzlemap
open Hints

val pointingPairs : puzzleMap -> cellCandidates -> hintDescription list

val boxLineReductions : puzzleMap -> cellCandidates -> hintDescription list
