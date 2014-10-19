module core.setCell

open sudoku

val setCellDigitApply : (Cell -> Set<Cell>) -> Value -> ((Cell -> CellContents) -> Cell -> CellContents)

val makeSetCellDigit : Cell -> Digit -> Value

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

val setCellDigitTry : Cell -> Digit -> (Cell -> CellContents) -> Either<Value, SetCellDigitError>
