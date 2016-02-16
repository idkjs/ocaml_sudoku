module hints.naked

open core.sudoku
open core.puzzlemap
open hints

val nakedSingleFind : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Cell list -> HintDescription2 list

val nakedNPerHouse : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> int -> House -> HintDescription2 list
