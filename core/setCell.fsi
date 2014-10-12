module core.setCell

open sudoku

val setCellSymbolApply : (Cell -> Set<Cell>) -> SetCellSymbolAction -> ((Cell -> CellContents) -> Cell -> CellContents)

val makeSetCellSymbol : Cell -> Candidate -> SetCellSymbolAction

type SetCellSymbolError = 
    { cell : Cell
      candidate : Candidate
      symbol : Symbol }

val setCellSymbolTry : Cell -> Candidate -> (Cell -> CellContents) -> Either<SetCellSymbolAction, SetCellSymbolError>
