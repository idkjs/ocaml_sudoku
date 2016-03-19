module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

val pointingPairs : puzzleMap -> cellCandidates -> hintDescription list

val boxLineReductions : puzzleMap -> cellCandidates -> hintDescription list
