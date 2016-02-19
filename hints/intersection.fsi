module hints.intersection

open core.sudoku
open core.puzzlemap
open hints

val pointingPairsPerBox : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> House -> Set<HintDescription2>

val boxLineReductionsPerHouse : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> MapCellBox -> House -> Set<HintDescription2>
