open Sudoku
open Puzzlemap
open Hints

val nakedSingle : puzzleMap -> cellCandidates -> hintDescription list

val nakedN : int -> puzzleMap -> cellCandidates -> hintDescription list
