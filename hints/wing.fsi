module hints.wing

open core.sudoku
open hints

val xWingFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
