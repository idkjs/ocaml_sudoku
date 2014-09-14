module hints.wing

open core.sudoku
open hints

val xWingFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> Row list -> Column list -> HintDescription list

val yWingFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> Row list -> Column list -> HintDescription list
