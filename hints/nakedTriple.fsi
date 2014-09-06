module hints.nakedTriple

open core.sudoku
open hints

val nakedTripleFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
