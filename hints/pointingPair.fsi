module hints.pointingPair

open core.sudoku
open hints

val pointingPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
