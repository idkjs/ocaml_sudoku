module core.setCell

open sudoku
open puzzlemap

let setCellDigitApply (cellHouseCells : CellHouseCells) (setCellValue : Value) : Current -> Current = 

    let update (cell : Cell) (cellContents : CellContents) : CellContents =
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = cellHouseCells.Get setCellValue.cell

            if setCellValue.cell = cell then BigNumber setCellValue.digit
            else if Set.contains cell cells then 
                PencilMarks(Set.remove setCellValue.digit candidates)
            else cellContents

    Map.map update

let makeSetCellDigit (cell : Cell) (digit : Digit) : Value = 
    { Value.cell = cell
      digit = digit }

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

let setCellDigitTry (cell : Cell) (candidate : Digit) (current : Current) : Either<Value, SetCellDigitError> = 
    match current.Item cell with
    | BigNumber digit -> 
        Right { SetCellDigitError.cell = cell
                candidate = candidate
                digit = digit }
    | PencilMarks _ ->
        Left(makeSetCellDigit cell candidate)
