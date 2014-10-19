module hints.wing

open core.sudoku
open hints

val xWingFind : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> Row list -> Column list -> HintDescription2 list

val yWingFind : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> Row list -> Column list -> HintDescription2 list
