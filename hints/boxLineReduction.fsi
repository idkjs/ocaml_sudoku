module hints.boxLineReduction

open core.sudoku
open hints

val boxLineReductionFind : (Cell -> Set<Candidate>)
     -> (House -> Set<Cell>) -> House list -> int<width> -> int<height> -> HintDescription list
