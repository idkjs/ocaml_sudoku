module hints.hidden

open core.sudoku
open core.puzzlemap
open hints

val hiddenNPerHouse : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> int -> House -> Set<HintDescription2>
