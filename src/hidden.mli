module hidden

open sudoku
open puzzlemap
open hints

val hiddenN : int -> puzzleMap -> cellCandidates -> hintDescription list
