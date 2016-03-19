module wing

open sudoku
open puzzlemap
open hints

val xWings : puzzleMap -> cellCandidates -> hintDescription list

val yWings : puzzleMap -> cellCandidates -> hintDescription list
