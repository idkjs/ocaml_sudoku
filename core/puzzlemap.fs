module core.puzzlemap

open sudoku

let columnCells (length : int<size>) (column : Column) = 
    List.map (fun row -> 
        { col = column
          row = row }) (rows length)

let rowCells (length : int<size>) (row : Row) = 
    List.map (fun column -> 
        { col = column
          row = row }) (columns length)

let columnStack (boxWidth : int<boxWidth>) (column : Column) = 
    1 + ((int) column.col - 1) / (int) boxWidth
    |> makeStack

let stackColumns (boxWidth : int<boxWidth>) (stack : Stack) = 
    let t = ((int) stack.stack - 1) * (int) boxWidth
    let ss = [ (t + 1)..(t + (int) boxWidth) ]
    List.map makeColumn ss

let rowBand (boxHeight : int<boxHeight>) (row : Row) = 
    1 + ((int) row.row - 1) / (int) boxHeight
    |> makeBand

let bandRows (boxHeight : int<boxHeight>) (band : Band) = 
    let c = ((int) band.band - 1) * (int) boxHeight
    let bb = [ (c + 1)..(c + (int) boxHeight) ]
    List.map makeRow bb

let cellBox (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (cell : Cell) = 
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { Box.band = band
      stack = stack }

let boxCells (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (box : Box) = 
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
          for column in stackColumns do
              yield { Cell.col = column
                      row = row } ]

let houseCells (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (house : House) = 
    match house with
    | HColumn c -> columnCells length c |> Set.ofList
    | HRow r -> rowCells length r |> Set.ofList
    | HBox b -> boxCells boxWidth boxHeight b |> Set.ofList

let houseCellCells (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (cell : Cell) = 
    let r = rowCells length cell.row |> Set.ofList

    let c = columnCells length cell.col |> Set.ofList

    let b = boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell) |> Set.ofList

    Set.unionMany [ r; c; b ]
