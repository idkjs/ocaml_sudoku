module hints.wing

open core.sudoku
open core.puzzlemap
open core.hints

val xWings : PuzzleMap -> CellCandidates -> Set<HintDescription>

val yWings : PuzzleMap -> CellCandidates -> Set<HintDescription>
