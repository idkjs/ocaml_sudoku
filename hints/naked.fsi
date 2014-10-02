module hints.naked

open core.sudoku
open hints

val nakedSingleFind : (Cell -> Set<Cell>) -> (House -> Set<Cell>) -> (Cell -> Set<Candidate>) -> Cell list -> HintDescription2 list

val nakedNPerHouse : (Cell -> Set<Cell>) -> (House -> Set<Cell>) -> (Cell -> Set<Candidate>) -> int -> House -> HintDescription2 list
