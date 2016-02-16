module hints.hidden

open core.sudoku
open core.puzzlemap
open hints

val hiddenNPerHouse : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> int -> House -> HintDescription2 list
