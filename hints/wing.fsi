module hints.wing

open core.sudoku
open core.puzzlemap
open hints

val xWingFind : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>

val yWingFind : Set<Cell> -> MapCellHouseCells
     -> (House -> Set<Cell>) -> MapCellCandidates -> Set<Row> -> Set<Column> -> Set<HintDescription2>
