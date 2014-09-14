module hints.hidden

open core.sudoku
open hints

val hiddenNPerHouse : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> int -> House -> HintDescription list
