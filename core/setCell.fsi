module core.setCell

open sudoku
open puzzlemap
open hints

val setCellDigitApply : CellHouseCells -> Value -> (Current -> Current)

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

val setCellDigitTry : Cell -> Digit -> Current -> Either<Value, SetCellDigitError>

val setCellHintDescription : PuzzleMap -> Value -> HintDescription2

val setCellStep : PuzzleMap -> Value -> Solution -> Solution
