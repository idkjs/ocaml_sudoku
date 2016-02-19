module hints.hidden

open core.sudoku
open core.puzzlemap
open hints

val hiddenNPerHouse : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> int -> House -> Set<HintDescription2>
