module hints.hidden

open core.sudoku
open core.puzzlemap
open core.hints

val hiddenN : int -> puzzleMap -> cellCandidates -> hintDescription list
