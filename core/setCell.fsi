module core.setCell

open puzzlemap
open sudoku

val setCellSymbolApply : (Cell -> Set<Cell>) -> SetCellSymbol -> ((Cell -> CellContents) -> Cell -> CellContents)

val makeSetCellSymbol : Cell-> Candidate -> SetCellSymbol

val setCellSymbolTry : Cell -> Candidate -> (Cell -> CellContents) -> SetCellSymbol option
