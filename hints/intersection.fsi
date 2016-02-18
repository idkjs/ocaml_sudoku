module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

val pointingPairsPerBox : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> House -> Set<HintDescription2>

val boxLineReductionsPerHouse : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> (Cell -> Box) -> House -> Set<HintDescription2>
