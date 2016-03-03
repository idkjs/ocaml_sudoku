module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

val nakedSingle : puzzleMap -> cellCandidates -> hintDescription array

val nakedN : int -> puzzleMap -> cellCandidates -> hintDescription array
