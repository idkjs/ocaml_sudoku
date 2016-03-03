module hints.wing

open core.sudoku
open core.puzzlemap
open core.hints

val xWings : puzzleMap -> cellCandidates -> hintDescription array

val yWings : puzzleMap -> cellCandidates -> hintDescription array
