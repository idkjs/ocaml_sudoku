module naked

open sudoku
open puzzlemap
open hints

val nakedSingle : puzzleMap -> cellCandidates -> hintDescription list

val nakedN : int -> puzzleMap -> cellCandidates -> hintDescription list
