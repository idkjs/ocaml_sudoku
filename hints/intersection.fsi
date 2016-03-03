module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

val pointingPairs : puzzleMap -> cellCandidates -> hintDescription array

val boxLineReductions : puzzleMap -> cellCandidates -> hintDescription array
