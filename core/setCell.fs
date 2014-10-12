module core.setCell

open sudoku

let setCellSymbolApply (cellHouseCells : Cell -> Set<Cell>) (setCellValue : SetCellSymbolAction) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell
        let cells = cellHouseCells cell

        match cellContents with
        | ASymbol _ -> cellContents
        | ACandidates candidates -> 
            if setCellValue.cell = cell then ASymbol setCellValue.symbol
            else if Set.contains cell cells then 
                ACandidates(Set.remove (symbolToCandidate setCellValue.symbol) candidates)
            else cellContents

let makeSetCellSymbol (cell : Cell) (candidate : Candidate) : SetCellSymbolAction = 
    let symbol = candidateToSymbol candidate

    { SetCellSymbolAction.cell = cell
      symbol = symbol }

type SetCellSymbolError = 
    { cell : Cell
      candidate : Candidate
      symbol : Symbol }

let setCellSymbolTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : Either<SetCellSymbolAction, SetCellSymbolError> = 
    match cellCellContents cell with
    | ASymbol symbol -> 
        Right { SetCellSymbolError.cell = cell
                candidate = candidate
                symbol = symbol }
    | ACandidates _ -> Left(makeSetCellSymbol cell candidate)
