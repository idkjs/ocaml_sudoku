module hints.intersection

open core.sudoku
open hints

val pointingPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> Box list -> HintDescription list

val boxLineReductionFind : (Cell -> Set<Candidate>)
     -> (House -> Set<Cell>) -> Row list -> Column list -> int<width> -> int<height> -> HintDescription list

