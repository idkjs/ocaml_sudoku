open Sudoku
open Puzzlemap
open hints

val nakedSingle : puzzleMap -> cellCandidates -> hintDescription list

val nakedN : int -> puzzleMap -> cellCandidates -> hintDescription list
