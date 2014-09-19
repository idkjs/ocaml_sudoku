module hints.hidden

open core.sudoku
open hints

val hiddenNPerHouse : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> int -> House -> HintDescription list
