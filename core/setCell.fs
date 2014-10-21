module core.setCell

open sudoku

let setCellDigitApply (cellHouseCells : Cell -> Set<Cell>) (setCellValue : Value) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell
        let cells = cellHouseCells setCellValue.cell

        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if setCellValue.cell = cell then BigNumber setCellValue.digit
            else if Set.contains cell cells then 
                PencilMarks(Set.remove setCellValue.digit candidates)
            else cellContents

let makeSetCellDigit (cell : Cell) (digit : Digit) : Value = 
    { Value.cell = cell
      digit = digit }

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

let setCellDigitTry (cell : Cell) (candidate : Digit) (cellCellContents : Cell -> CellContents) : Either<Value, SetCellDigitError> = 
    match cellCellContents cell with
    | BigNumber digit -> 
        Right { SetCellDigitError.cell = cell
                candidate = candidate
                digit = digit }
    | PencilMarks _ -> Left(makeSetCellDigit cell candidate)
