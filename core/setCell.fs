module core.setCell

open sudoku

let setCellDigitApply (cellHouseCells : Cell -> Set<Cell>) (setCellValue : SetCellDigitAction) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell
        let cells = cellHouseCells cell

        match cellContents with
        | ADigit _ -> cellContents
        | ACandidates candidates -> 
            if setCellValue.cell = cell then ADigit setCellValue.digit
            else if Set.contains cell cells then 
                ACandidates(Set.remove (digitToCandidate setCellValue.digit) candidates)
            else cellContents

let makeSetCellDigit (cell : Cell) (candidate : Candidate) : SetCellDigitAction = 
    let Digit = candidateToDigit candidate

    { SetCellDigitAction.cell = cell
      digit = Digit }

type SetCellDigitError = 
    { cell : Cell
      candidate : Candidate
      digit : Digit }

let setCellDigitTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : Either<SetCellDigitAction, SetCellDigitError> = 
    match cellCellContents cell with
    | ADigit digit -> 
        Right { SetCellDigitError.cell = cell
                candidate = candidate
                digit = digit }
    | ACandidates _ -> Left(makeSetCellDigit cell candidate)
