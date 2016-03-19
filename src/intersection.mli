module intersection

open sudoku
open puzzlemap
open hints

val pointingPairs : puzzleMap -> cellCandidates -> hintDescription list

val boxLineReductions : puzzleMap -> cellCandidates -> hintDescription list
