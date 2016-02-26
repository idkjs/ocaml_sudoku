module core.puzzlemap

open sudoku

let makeColumn (i : int) : column =
    i |> CColumn

let makeRow (i : int) : row =
    i |> RRow

let makeStack (i : int) : stack =
    i |> SStack

let makeBand (i : int) : band =
    i |> BBand

let makeSetCellDigit (cell : cell) (digit : digit) : value = 
    { value.cell = cell
      digit = digit }

let makeCandidate (cell : cell) (digit : digit) : candidate =
    { candidate.cell = cell
      digit = digit }

type columnCells = lookup<column, Set<cell>>

type rowCells = lookup<row, Set<cell>>

type columnStack = lookup<column, stack>

type stackColumns = lookup<stack, Set<column>>

type rowBand = lookup<row, band>

type bandRows = lookup<band, Set<row>>

type cellBox = lookup<cell, box>

type boxCells = lookup<box, Set<cell>>

// for a house, return the cells in it
type houseCells = lookup<house, Set<cell>>

// for a cell, return the cells in the column, row and box it belongs to
type cellHouseCells =lookup<cell, Set<cell>>

let makeMapLookup<'a, 'b when 'a : comparison and 'b : comparison> (as' : Set<'a>) (fn : 'a -> 'b) : mapLookup<'a, 'b> =
    as'
    |> Set.map (fun a -> (a, fn a))
    |> Map.ofSeq
    |> mapLookup<'a, 'b>

let orderedColumns (length : size) : column list =
    [ 1..(int) length ]
    |> List.map makeColumn

let orderedRows (length : size) : row list =
    [ 1..(int) length ]
    |> List.map makeRow

let orderedCells (length : size) : cell list =
    [ for row in (orderedRows length) do
          for column in (orderedColumns length) do
              yield { cell.col = column
                      row = row } ]

let orderedStacks (length : size) (boxWidth : boxWidth) : stack list =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack

let orderedBands (length : size) (boxHeight : boxHeight) : band list =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand

let orderedStackColumns (boxWidth : boxWidth) (stack : stack) : column list =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn

let orderedBandRows (boxHeight : boxHeight) (band : band) : row list =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight

    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow

let columns (length : size) : Set<column> =
    [ 1..(int) length ]
    |> List.map makeColumn
    |> Set.ofList

let rows (length : size) : Set<row> =
    [ 1..(int) length ]
    |> List.map makeRow
    |> Set.ofList

let cells (length : size) : Set<cell> =
    orderedCells length
    |> Set.ofList

let stacks (length : size) (boxWidth : boxWidth) : Set<stack> =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack
    |> Set.ofList

let bands (length : size) (boxHeight : boxHeight) : Set<band> =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand
    |> Set.ofList

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : Set<box> =
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { box.stack = stack
                      band = band } ]
    |> Set.ofList

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : Set<house> =
    let chs =
        columns length
        |> Set.map HColumn

    let rhs : Set<house> =
        rows length
        |> Set.map HRow

    let bhs : Set<house> =
        boxes length boxWidth boxHeight
        |> Set.map HBox

    [ chs; rhs; bhs ]
    |> Set.unionMany

let columnCells (length : size) (column : column) : Set<cell> =
    rows length
    |> Set.map (fun row -> 
        { col = column
          row = row })

let rowCells (length : size) (row : row) : Set<cell> =
    columns length
    |> Set.map (fun column -> 
        { col = column
          row = row })

let columnStack (boxWidth : boxWidth) (column : column) : stack =
    match column with
    | CColumn c ->
        1 + ((int) c- 1) / (int) boxWidth
        |> makeStack

let stackColumns (boxWidth : boxWidth) (stack : stack) : Set<column> =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn
        |> Set.ofList

let rowBand (boxHeight : boxHeight) (row : row) : band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : boxHeight) (band : band) : Set<row> =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight
    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow
    |> Set.ofList

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : box =
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { box.band = band
      stack = stack }

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : Set<cell> =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
          for column in stackColumns do
              yield { cell.col = column
                      row = row } ]
    |> Set.ofList

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : house) : Set<cell> =
    match house with
    | HColumn c -> columnCells length c
    | HRow r -> rowCells length r
    | HBox b -> boxCells boxWidth boxHeight b

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : Set<cell> =
    let r : Set<cell> =
        rowCells length cell.row

    let c : Set<cell> =
        columnCells length cell.col

    let b : Set<cell> =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)

    [ r; c; b ]
    |> Set.unionMany


type puzzleMap =
    // The order they are normally written in
    abstract member orderedColumns : column list
    abstract member orderedRows : row list
    abstract member orderedCells : cell list
    abstract member orderedStacks : stack list
    abstract member orderedBands : band list

    // for a stack, return the columns in it
    abstract member orderedStackColumns : stack -> column list

    // for a band, return the rows in it
    abstract member orderedBandRows : band -> row list

    abstract member columns : Set<column>

    abstract member rows : Set<row>

    abstract member cells : Set<cell>

    abstract member stacks : Set<stack>

    abstract member bands : Set<band>

    abstract member boxes : Set<box>

    abstract member houses : Set<house>

    // for a column, return the cells in it
    abstract member columnCells : columnCells

    // for a row, return the cells in it
    abstract member rowCells : rowCells

    // for a column, which stack is it in?
    abstract member columnStack : columnStack

    // for a stack, return the columns in it
    abstract member stackColumns : stackColumns

    // for a row, which band is it in?
    abstract member rowBand : rowBand

    // for a band, return the rows in it
    abstract member bandRows : bandRows

    // for a cell, which box is it in?
    abstract member cellBox : cellBox

    // for a box, return the cells in it
    abstract member boxCells : boxCells

    // for a house, return the cells in it
    abstract member houseCells : houseCells

    abstract member cellHouseCells : cellHouseCells

type tPuzzleMap(puzzleShape : puzzleShape) =

    let _orderedColumns = orderedColumns puzzleShape.size
    let _orderedRows = orderedRows puzzleShape.size
    let _orderedCells = orderedCells puzzleShape.size
    let _orderedStacks = orderedStacks puzzleShape.size puzzleShape.boxWidth
    let _orderedBands = orderedBands puzzleShape.size puzzleShape.boxHeight

    let _columns = columns puzzleShape.size
    let _rows = rows puzzleShape.size
    let _cells = cells puzzleShape.size
    let _stacks = stacks puzzleShape.size puzzleShape.boxWidth
    let _bands = bands puzzleShape.size puzzleShape.boxHeight
    let _boxes = boxes puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight
    let _houses = houses puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight

    let _columnCells =
        makeMapLookup<column, Set<cell>> _columns (fun column -> columnCells puzzleShape.size column)
        :> columnCells

    let _rowCells =
        makeMapLookup<row, Set<cell>> _rows (fun row -> rowCells puzzleShape.size row)
        :> rowCells

    let _columnStack =
        makeMapLookup<column, stack> _columns (fun column -> columnStack puzzleShape.boxWidth column)
        :> columnStack

    let _stackColumns =
        makeMapLookup<stack, Set<column>> _stacks (fun stack -> stackColumns puzzleShape.boxWidth stack)
        :> stackColumns

    let _rowBand =
        makeMapLookup<row, band> _rows (fun row -> rowBand puzzleShape.boxHeight row)
        :> rowBand

    let _bandRows =
        makeMapLookup<band, Set<row>> _bands (fun band -> bandRows puzzleShape.boxHeight band)
        :> bandRows

    let _cellBox =
        makeMapLookup<cell, box> _cells (fun cell -> cellBox puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> cellBox

    let _boxCells =
        makeMapLookup<box, Set<cell>> _boxes (fun box -> boxCells puzzleShape.boxWidth puzzleShape.boxHeight box)
        :> boxCells

    let _houseCells =
        makeMapLookup<house, Set<cell>> _houses (fun house -> houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight house)
        :> houseCells

    let _cellHouseCells =
        makeMapLookup<cell, Set<cell>> _cells (fun cell -> cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> cellHouseCells

    interface puzzleMap with

        // The order they are normally written in
        member this.orderedColumns : column list = _orderedColumns
        member this.orderedRows : row list = _orderedRows
        member this.orderedCells : cell list = _orderedCells
        member this.orderedStacks : stack list = _orderedStacks
        member this.orderedBands : band list = _orderedBands

        // for a stack, return the columns in it
        member this.orderedStackColumns (stack : stack) : column list = orderedStackColumns puzzleShape.boxWidth stack

        // for a band, return the rows in it
        member this.orderedBandRows (band : band) : row list = orderedBandRows puzzleShape.boxHeight band

        member this.columns : Set<column> = _columns

        member this.rows : Set<row> = _rows

        member this.cells : Set<cell> = _cells

        member this.stacks : Set<stack> = _stacks

        member this.bands : Set<band> = _bands

        member this.boxes : Set<box> = _boxes

        member this.houses : Set<house> = _houses

        // for a column, return the cells in it
        member this.columnCells : columnCells = _columnCells

        // for a row, return the cells in it
        member this.rowCells : rowCells = _rowCells

        // for a column, which stack is it in?
        member this.columnStack : columnStack = _columnStack

        // for a stack, return the columns in it
        member this.stackColumns : stackColumns = _stackColumns

        // for a row, which band is it in?
        member this.rowBand : rowBand = _rowBand

        // for a band, return the rows in it
        member this.bandRows : bandRows = _bandRows

        // for a cell, which box is it in?
        member this.cellBox : cellBox = _cellBox

        // for a box, return the cells in it
        member this.boxCells : boxCells = _boxCells

        // for a house, return the cells in it
        member this.houseCells : houseCells = _houseCells

        member this.cellHouseCells : cellHouseCells = _cellHouseCells
