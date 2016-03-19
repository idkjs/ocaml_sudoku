module hints.wing

open core.sudoku
open core.puzzlemap
open core.hints

val xWings : puzzleMap -> cellCandidates -> hintDescription list

val yWings : puzzleMap -> cellCandidates -> hintDescription list
