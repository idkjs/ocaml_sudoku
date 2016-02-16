module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

val pointingPairsPerBox : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> House -> HintDescription2 list

val boxLineReductionsPerHouse : MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> (Cell -> Box) -> House -> HintDescription2 list
