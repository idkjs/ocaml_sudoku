module hints.nakedPair

open core.sudoku
open hints

val nakedPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
