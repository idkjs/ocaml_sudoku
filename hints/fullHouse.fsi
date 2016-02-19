module hints.fullHouse

open core.sudoku
open core.puzzlemap

open hints

val fullHousePerHouse : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> House -> Set<HintDescription2>
