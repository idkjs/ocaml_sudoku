module hints.naked

open core.sudoku
open hints

val nakedSingleFind : (Cell -> Set<Candidate>) -> Cell list -> HintDescription list

val nakedNPerHouse : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> int -> House -> HintDescription list
