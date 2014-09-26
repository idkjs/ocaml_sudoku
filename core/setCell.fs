module core.setCell

open System

open core.sudoku

let setCellApply (setCellValue : SetCellValue) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (entryLookup : Cell -> CellContents) (cell : Cell) -> 
        let entry = entryLookup cell

        match entry with
        | ASymbol _ -> entry
        | ACandidates candidates -> 
            let candidateToSymbol (Candidate s : Candidate) = ASymbol(Symbol s)
            if setCellValue.cell = cell then candidateToSymbol setCellValue.candidate
            else if Set.contains cell setCellValue.reductions then ACandidates(Set.remove setCellValue.candidate candidates)
            else entry

let makeSetCellValue (cell : Cell) (candidate : Candidate) (cellHouseCells : Cell -> Set<Cell>) (candidateLookup : Cell -> Set<Candidate>) : SetCellValue =

    let houseCells = cellHouseCells cell
    let otherHouseCells = Set.remove cell houseCells

    let reductions = Set.filter (candidateLookup >> Set.contains candidate) otherHouseCells

    { SetCellValue.cell = cell
      reductions = reductions
      candidate = candidate }

let setCellTry (candidate : Candidate) (candidateLookup : Cell -> Set<Candidate>) (cellHouseCells : Cell -> Set<Cell>) (entryLookup : Cell -> CellContents) (cell : Cell) : SetCellValue option= 
    match entryLookup cell with
    | ASymbol symbol -> 
        Console.WriteLine("Cell {0} has been set value {1}", cell, symbol)
        None
    | ACandidates _ -> 
        Some (makeSetCellValue cell candidate cellHouseCells candidateLookup)
