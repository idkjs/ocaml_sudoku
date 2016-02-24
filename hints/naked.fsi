module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

val nakedSingle : PuzzleMap -> CellCandidates -> Set<HintDescription>

val nakedN : int -> PuzzleMap -> CellCandidates -> Set<HintDescription>
