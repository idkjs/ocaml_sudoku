module hints.wing

open core.sudoku
open core.puzzlemap
open hints

val xWingFind : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>

val yWingFind : Set<Cell> -> CellHouseCells
     -> HouseCells -> CellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>
