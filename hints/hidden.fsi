module hints.hidden

open core.sudoku
open hints

val hiddenSingleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val hiddenPairFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val hiddenTripleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val hiddenQuadFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
