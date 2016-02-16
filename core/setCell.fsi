module core.setCell

open sudoku
open puzzlemap

val setCellDigitApply : MapCellHouseCells -> Value -> (Current -> Current)

val makeSetCellDigit : Cell -> Digit -> Value

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

val setCellDigitTry : Cell -> Digit -> Current -> Either<Value, SetCellDigitError>
