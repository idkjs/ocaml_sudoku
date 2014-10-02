module hints.intersection

open core.sudoku
open hints

val pointingPairsPerBox : (Cell -> Set<Cell>) -> (House -> Set<Cell>) -> (Cell -> Set<Candidate>) -> House -> HintDescription2 list

val boxLineReductionsPerHouse : (Cell -> Set<Cell>) -> (House -> Set<Cell>) -> (Cell -> Set<Candidate>)
     -> int<width> -> int<height> -> House -> HintDescription2 list
