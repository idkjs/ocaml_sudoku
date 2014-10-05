module core.setCell

open System

open core.sudoku

let setCellSymbolApply (cellHouseCells : Cell -> Set<Cell>) (setCellValue : SetCellSymbol) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell
        let cells = cellHouseCells cell

        match cellContents with
        | ASymbol _ -> cellContents
        | ACandidates candidates -> 
            if setCellValue.cell = cell then ASymbol setCellValue.symbol
            else if Set.contains cell cells then ACandidates(Set.remove (symbolToCandidate setCellValue.symbol) candidates)
            else cellContents

let makeSetCellSymbol (cell : Cell) (candidate : Candidate) : SetCellSymbol =
    let symbol = candidateToSymbol candidate

    { SetCellSymbol.cell = cell
      symbol = symbol }

let setCellSymbolTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : SetCellSymbol option= 
    match cellCellContents cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates _ -> 
        Some (makeSetCellSymbol cell candidate)
