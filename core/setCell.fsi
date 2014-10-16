module core.setCell

open sudoku

val setCellDigitApply : (Cell -> Set<Cell>) -> SetCellDigitAction -> ((Cell -> CellContents) -> Cell -> CellContents)

val makeSetCellDigit : Cell -> Candidate -> SetCellDigitAction

type SetCellDigitError = 
    { cell : Cell
      candidate : Candidate
      digit : Digit }

val setCellDigitTry : Cell -> Candidate -> (Cell -> CellContents) -> Either<SetCellDigitAction, SetCellDigitError>
