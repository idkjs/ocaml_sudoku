module hints.hidden

open core.sudoku
open core.puzzlemap
open hints

val hiddenN : int -> PuzzleMap -> CellCandidates -> Set<HintDescription2>
