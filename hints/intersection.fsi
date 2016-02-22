module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

val pointingPairs : PuzzleMap -> CellCandidates -> Set<HintDescription2>

val boxLineReductions : PuzzleMap -> CellCandidates -> Set<HintDescription2>
