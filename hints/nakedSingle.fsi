module hints.nakedSingle

open core.sudoku
open hints

val nakedSingleFind : (Cell -> Set<Candidate>) -> Cell list -> HintDescription list
