module hints.naked

open core.sudoku
open core.puzzlemap
open hints

val nakedSingleFind : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Set<Cell> -> Set<HintDescription2>

val nakedNPerHouse : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> int -> House -> Set<HintDescription2>
