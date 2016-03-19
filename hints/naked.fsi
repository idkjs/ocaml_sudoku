module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

val nakedSingle : puzzleMap -> cellCandidates -> hintDescription list

val nakedN : int -> puzzleMap -> cellCandidates -> hintDescription list
