module hints.fullHouse

open core.sudoku
open hints

val fullHousePerHouse : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House -> HintDescription list
