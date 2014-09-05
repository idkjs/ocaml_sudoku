module hints.hiddenQuad

open core.sudoku
open hints

val hiddenQuadFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
