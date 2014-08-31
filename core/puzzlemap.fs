module core.puzzlemap

open sudoku

let konst x _ = x

let symbolToCandidate (Symbol s : Symbol) = Candidate s

let makeColumn i = { Column.col = i * 1<column> }

let makeRow i = { Row.row = i * 1<row> }

let makeStack i = { Stack.stack = i * 1<boxcol> }

let makeBand i = { Band.band = i * 1<boxrow> }

let columns (length : int) = List.map makeColumn [ 1..length ]

let rows (length : int) = List.map makeRow [ 1..length ]

let cells (length : int) =
    [ for row in (rows length) do
            for column in (columns length) do
                yield { Cell.col = column
                        row = row } ]

let columnCells (length : int) (column : Column) = 
    List.map (fun row -> 
        { col = column
          row = row }) (rows length)

// For a cell, return all the cell symbols for its containing row
let columnCellCells (length : int) (cell : Cell) = columnCells length cell.col

let rowCells (length : int) (row : Row ) = 
    List.map (fun column -> 
        { col = column
          row = row }) (columns length)

// For a cell, return all the cell symbols for its containing column
let rowCellCells (length : int) (cell : Cell) = rowCells length cell.row

let stacks (length : int) (boxWidth : int<width>) = 
    List.map makeStack [ 1..(length / (int) boxWidth) ]

let columnStack (boxWidth : int<width>) (column : Column) = 
    1 + ((int) column.col - 1) / (int) boxWidth
    |> makeStack

let stackColumns (boxWidth : int<width>) (stack : Stack) = 
    let t = ((int) stack.stack - 1) * (int) boxWidth
    let ss = [ (t + 1)..(t + (int) boxWidth) ]
    List.map makeColumn ss

let bands (length : int) (boxHeight : int<height>) = 
    List.map makeBand [ 1..(length / (int) boxHeight) ]

let rowBand (boxHeight : int<height>) (row : Row) = 
    1 + ((int) row.row - 1) / (int) boxHeight
    |> makeBand

let bandRows (boxHeight : int<height>) (band : Band) = 
    let c = ((int) band.band - 1) * (int) boxHeight
    let bb = [ (c + 1)..(c + (int) boxHeight) ]
    List.map makeRow bb

let boxes (length : int) (boxWidth : int<width>) (boxHeight : int<height>) =
    [ for band in bands length boxHeight do
            for stack in stacks length boxWidth do
                yield { Box.stack = stack
                        band = band } ]

let cellBox (boxWidth : int<width>) (boxHeight : int<height>) (cell : Cell) = 
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { Box.band = band
      stack = stack }

let boxCells (boxWidth : int<width>) (boxHeight : int<height>) (box : Box) = 
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
            for column in stackColumns do
                yield { Cell.col = column
                        row = row } ]


// For a cell, return all the cell symbols for its containing box
let boxCellCells (boxWidth : int<width>) (boxHeight : int<height>) (cell : Cell) = boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)

let houses (length : int) (boxWidth : int<width>) (boxHeight : int<height>) = 
    let chs = List.map (fun c -> Column c) (columns length)

    let rhs = List.map (fun r -> Row r) (rows length)

    let bhs = List.map (fun b -> Box b) (boxes length boxWidth boxHeight)

    List.concat [ chs; rhs; bhs ]

let houseCells (length : int) (boxWidth : int<width>) (boxHeight : int<height>) (house : House) = 
    match house with
    | Column c -> columnCells length c |> Set.ofList
    | Row r -> rowCells length r |> Set.ofList
    | Box b -> boxCells boxWidth boxHeight b |> Set.ofList

let houseCellCells (length : int) (boxWidth : int<width>) (boxHeight : int<height>) (cell : Cell) = 
    let r = 
        cell
        |> rowCellCells length
        |> Set.ofList
    
    let c = 
        cell
        |> columnCellCells length
        |> Set.ofList
    
    let b = 
        cell
        |> boxCellCells boxWidth boxHeight
        |> Set.ofList
    
    Set.unionMany [ r; c; b ]

let houseCellsForCell (length : int) (boxWidth : int<width>) (boxHeight : int<height>) (cell : Cell) (symbolLookup : Cell -> 'a option) = 
    let a = houseCellCells length boxWidth boxHeight cell

    let sys = Set.map symbolLookup a
    let sys2 = Set.filter (fun (s : 'a option) -> s.IsSome) sys
    Set.map (fun (s : 'a option) -> s.Value) sys2
