module core.puzzlemap

open sudoku

let konst x _ = x

let symbolToCandidate (Symbol s : Symbol) = Candidate s

let columnCells (length : int<size>) (column : Column) = 
    List.map (fun row -> 
        { col = column
          row = row }) (rows length)

let rowCells (length : int<size>) (row : Row) = 
    List.map (fun column -> 
        { col = column
          row = row }) (columns length)

let columnStack (boxWidth : int<width>) (column : Column) = 
    1 + ((int) column.col - 1) / (int) boxWidth
    |> makeStack

let stackColumns (boxWidth : int<width>) (stack : Stack) = 
    let t = ((int) stack.stack - 1) * (int) boxWidth
    let ss = [ (t + 1)..(t + (int) boxWidth) ]
    List.map makeColumn ss

let rowBand (boxHeight : int<height>) (row : Row) = 
    1 + ((int) row.row - 1) / (int) boxHeight
    |> makeBand

let bandRows (boxHeight : int<height>) (band : Band) = 
    let c = ((int) band.band - 1) * (int) boxHeight
    let bb = [ (c + 1)..(c + (int) boxHeight) ]
    List.map makeRow bb

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

let houseCells (length : int<size>) (boxWidth : int<width>) (boxHeight : int<height>) (house : House) = 
    match house with
    | Column c -> columnCells length c |> Set.ofList
    | Row r -> rowCells length r |> Set.ofList
    | Box b -> boxCells boxWidth boxHeight b |> Set.ofList

let houseCellCells (length : int<size>) (boxWidth : int<width>) (boxHeight : int<height>) (cell : Cell) = 
    let r = rowCells length cell.row |> Set.ofList

    let c = columnCells length cell.col |> Set.ofList

    let b = boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell) |> Set.ofList

    Set.unionMany [ r; c; b ]
