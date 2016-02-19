module hints.fullHouse

open core.sudoku
open core.puzzlemap

open hints

val fullHousePerHouse : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> House -> Set<HintDescription2>
