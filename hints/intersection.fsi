module hints.intersection

open core.sudoku
open hints

val pointingPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val boxLineReductionFind : (Cell -> Set<Candidate>)
     -> (House -> Set<Cell>) -> House list -> int<width> -> int<height> -> HintDescription list

