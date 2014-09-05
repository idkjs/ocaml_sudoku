module hints.hiddenTriple

open core.sudoku
open hints

val hiddenTripleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
