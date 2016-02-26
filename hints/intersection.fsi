module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

val pointingPairs : puzzleMap -> cellCandidates -> Set<hintDescription>

val boxLineReductions : puzzleMap -> cellCandidates -> Set<hintDescription>
