module hints.wing

open core.sudoku
open core.puzzlemap
open hints

val xWingFind : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Row list -> Column list -> HintDescription2 list

val yWingFind : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Row list -> Column list -> HintDescription2 list
