module core.setCell

open puzzlemap
open sudoku

val setCellApply : SetCellValue -> ((Cell -> CellContents) -> Cell -> CellContents)

val makeSetCellValue : Cell-> Candidate -> (Cell -> Set<Cell>) -> SetCellValue

val setCellTry : Candidate -> (Cell -> Set<Cell>) -> (Cell -> CellContents) -> Cell -> SetCellValue option
