module hints.naked

open core.sudoku
open core.puzzlemap
open hints

val nakedSingle : PuzzleMap -> CellCandidates -> Set<HintDescription2>

val nakedN : int -> PuzzleMap -> CellCandidates -> Set<HintDescription2>
