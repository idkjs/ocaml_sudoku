module hints.hiddenSingle

open core.sudoku
open hints

val hiddenSingleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
