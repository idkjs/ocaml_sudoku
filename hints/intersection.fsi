module hints.intersection

open core.sudoku
open hints

val pointingPairsPerBox : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> House -> HintDescription2 list

val boxLineReductionsPerHouse : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> (Cell -> Box) -> House -> HintDescription2 list
