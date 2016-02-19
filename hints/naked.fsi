module hints.naked

open core.sudoku
open core.puzzlemap
open hints

val nakedSingleFind : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> Set<Cell> -> Set<HintDescription2>

val nakedNPerHouse : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> int -> House -> Set<HintDescription2>
