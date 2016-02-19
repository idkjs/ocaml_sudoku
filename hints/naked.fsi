module hints.naked

open core.sudoku
open core.puzzlemap
open hints

val nakedSingleFind : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> Set<Cell> -> Set<HintDescription2>

val nakedNPerHouse : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> int -> House -> Set<HintDescription2>
