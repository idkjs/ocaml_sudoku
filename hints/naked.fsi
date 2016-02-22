module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

val nakedSingle : PuzzleMap -> CellCandidates -> Set<HintDescription2>

val nakedN : int -> PuzzleMap -> CellCandidates -> Set<HintDescription2>
