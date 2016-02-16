module core.puzzlemap

open sudoku

let makeColumn i = { Column.col = i * 1<column> }

let columns (length : int<size>) : Column list = List.map makeColumn [ 1..(int) length ]

let makeRow i = { Row.row = i * 1<row> }

let rows (length : int<size>) = List.map makeRow [ 1..(int) length ]

let cells (length : int<size>) = 
    [ for row in (rows length) do
          for column in (columns length) do
              yield { Cell.col = column
                      row = row } ]

let makeStack i = { Stack.stack = i * 1<stack> }

let stacks (length : int<size>) (boxWidth : int<boxWidth>) = List.map makeStack [ 1..((int) length / (int) boxWidth) ]

let makeBand i = { Band.band = i * 1<band> }

let bands (length : int<size>) (boxHeight : int<boxHeight>) = List.map makeBand [ 1..((int) length / (int) boxHeight) ]

let boxes (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) = 
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { Box.stack = stack
                      band = band } ]

let houses (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) = 
    let chs = List.map (fun c -> HColumn c) (columns length)

    let rhs = List.map (fun r -> HRow r) (rows length)

    let bhs = List.map (fun b -> HBox b) (boxes length boxWidth boxHeight)

    List.concat [ chs; rhs; bhs ]

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

type MapCellHouseCells = Cell -> Set<Cell>

let houseCellCells (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) : MapCellHouseCells =
    fun (cell : Cell) ->
        let r = rowCells length cell.row |> Set.ofList

        let c = columnCells length cell.col |> Set.ofList

        let b = boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell) |> Set.ofList

        Set.unionMany [ r; c; b ]
