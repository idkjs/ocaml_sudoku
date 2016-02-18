module core.puzzlemap

open sudoku

let makeColumn (i : int) : Column =
    i * 1<column> |> CColumn

let makeRow (i : int) : Row =
    i * 1<row> |> RRow

let makeStack (i : int) : Stack =
    i * 1<stack> |> SStack

let makeBand (i : int) : Band =
    i * 1<band> |> BBand

let orderedColumns (length : int<size>) : Column list =
    [ 1..(int) length ]
    |> List.map makeColumn

let orderedRows (length : int<size>) : Row list =
    [ 1..(int) length ]
    |> List.map makeRow

let orderedCells (length : int<size>) : Cell list =
    [ for row in (orderedRows length) do
          for column in (orderedColumns length) do
              yield { Cell.col = column
                      row = row } ]

let orderedStacks (length : int<size>) (boxWidth : int<boxWidth>) : Stack list =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack

let orderedBands (length : int<size>) (boxHeight : int<boxHeight>) : Band list =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand

let orderedStackColumns (boxWidth : int<boxWidth>) (stack : Stack) : Column list =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn

let orderedBandRows (boxHeight : int<boxHeight>) (band : Band) : Row list =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight

    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow

let columns (length : int<size>) : Set<Column> =
    [ 1..(int) length ]
    |> List.map makeColumn
    |> Set.ofList

let rows (length : int<size>) : Set<Row> =
    [ 1..(int) length ]
    |> List.map makeRow
    |> Set.ofList

let cells (length : int<size>) : Set<Cell> =
    orderedCells length
    |> Set.ofList

let stacks (length : int<size>) (boxWidth : int<boxWidth>) : Set<Stack> =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack
    |> Set.ofList

let bands (length : int<size>) (boxHeight : int<boxHeight>) : Set<Band> =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand
    |> Set.ofList

let boxes (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) : Set<Box> =
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { Box.stack = stack
                      band = band } ]
    |> Set.ofList

let houses (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) : Set<House> =
    let chs =
        columns length
        |> Set.map HColumn

    let rhs : Set<House> =
        rows length
        |> Set.map HRow

    let bhs : Set<House> =
        boxes length boxWidth boxHeight
        |> Set.map HBox

    [ chs; rhs; bhs ]
    |> Set.unionMany

let columnCells (length : int<size>) (column : Column) : Set<Cell> =
    rows length
    |> Set.map (fun row -> 
        { col = column
          row = row })

let rowCells (length : int<size>) (row : Row) : Set<Cell> =
    columns length
    |> Set.map (fun column -> 
        { col = column
          row = row })

let columnStack (boxWidth : int<boxWidth>) (column : Column) : Stack =
    match column with
    | CColumn c ->
        1 + ((int) c- 1) / (int) boxWidth
        |> makeStack

let stackColumns (boxWidth : int<boxWidth>) (stack : Stack) : Set<Column> =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn
        |> Set.ofList

let rowBand (boxHeight : int<boxHeight>) (row : Row) : Band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : int<boxHeight>) (band : Band) : Set<Row> =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight
    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow
    |> Set.ofList

let cellBox (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (cell : Cell) : Box =
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { Box.band = band
      stack = stack }

let boxCells (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (box : Box) : Set<Cell> =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
          for column in stackColumns do
              yield { Cell.col = column
                      row = row } ]
    |> Set.ofList

let houseCells (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) (house : House) : Set<Cell> =
    match house with
    | HColumn c -> columnCells length c
    | HRow r -> rowCells length r
    | HBox b -> boxCells boxWidth boxHeight b

type MapCellHouseCells = Map<Cell, Set<Cell>>

let houseCellCells (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) : MapCellHouseCells =
    let lookup (cell : Cell) : Set<Cell> =
        let r : Set<Cell> =
            rowCells length cell.row

        let c : Set<Cell> =
            columnCells length cell.col

        let b : Set<Cell> =
            boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)

        [ r; c; b ]
        |> Set.unionMany

    cells length
    |> Set.map (fun cell -> (cell, lookup cell))
    |> Map.ofSeq
