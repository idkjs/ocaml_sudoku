open Sudoku
open Puzzlemap

val pointingPairs : puzzleMap -> cellCandidates -> Hint.description list

val boxLineReductions : puzzleMap -> cellCandidates -> Hint.description list
