module hints.wing

open core.sudoku
open core.puzzlemap
open hints

val xWingFind : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>

val yWingFind : Set<Cell> -> MapCellHouseCells
     -> MapHouseCells -> MapCellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>
