module hints.intersection

open core.sudoku
open hints

val pointingPairsPerBox : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House -> HintDescription list
val boxLineReductionsPerHouse : (Cell -> Set<Candidate>)
     -> (House -> Set<Cell>) -> int<width> -> int<height> -> House -> HintDescription list
