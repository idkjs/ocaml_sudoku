module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

val pointingPairsPerBox : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> House -> Set<HintDescription2>

val boxLineReductionsPerHouse : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> CellBox -> House -> Set<HintDescription2>
