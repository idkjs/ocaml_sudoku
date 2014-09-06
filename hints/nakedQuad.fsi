module hints.nakedQuad

open core.sudoku
open hints

val nakedQuadFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
