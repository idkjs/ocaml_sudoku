module hints.naked

open core.sudoku
open hints

val nakedSingleFind : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> Cell list -> HintDescription2 list

val nakedNPerHouse : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> int -> House -> HintDescription2 list
