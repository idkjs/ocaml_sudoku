module hints.hidden

open core.sudoku
open hints

val hiddenNPerHouse : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Candidate>) -> int -> House -> HintDescription2 list
