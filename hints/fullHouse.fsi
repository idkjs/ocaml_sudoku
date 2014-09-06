module hints.fullHouse

open core.sudoku
open hints

val fullHouseFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HintDescription list
