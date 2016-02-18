module hints.fullHouse

open core.sudoku
open core.puzzlemap

open hints

val fullHousePerHouse : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> House -> Set<HintDescription2>
