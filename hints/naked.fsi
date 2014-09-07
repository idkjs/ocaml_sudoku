module hints.naked

open core.sudoku
open hints

val nakedSingleFind : (Cell -> Set<Candidate>) -> Cell list -> HintDescription list

val nakedPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val nakedTripleFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list

val nakedQuadFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
