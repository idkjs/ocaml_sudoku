module hints.fullHouse

open core.sudoku
open core.puzzlemap

open hints

val fullHousePerHouse : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> House -> HintDescription2 list
