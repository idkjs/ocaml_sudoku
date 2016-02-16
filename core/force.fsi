module core.force

open sudoku
open puzzlemap

val solve : Solution -> Cell list -> MapCellHouseCells -> Solution list
