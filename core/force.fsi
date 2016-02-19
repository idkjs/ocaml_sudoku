module core.force

open sudoku
open puzzlemap

val solve : Solution -> Set<Cell> -> CellHouseCells -> Set<Solution>
