module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

val pointingPairs : PuzzleMap -> CellCandidates -> Set<HintDescription>

val boxLineReductions : PuzzleMap -> CellCandidates -> Set<HintDescription>
