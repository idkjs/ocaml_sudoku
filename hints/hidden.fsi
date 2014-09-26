module hints.hidden

open core.sudoku
open hints

val hiddenNPerHouse : (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> int -> House -> HintDescription list
