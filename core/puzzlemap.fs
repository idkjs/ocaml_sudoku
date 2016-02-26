module core.puzzlemap

open sudoku

let makeColumn (i : int) : Column =
    i |> CColumn

let makeRow (i : int) : Row =
    i |> RRow

let makeStack (i : int) : Stack =
    i |> SStack

let makeBand (i : int) : Band =
    i |> BBand

let makeSetCellDigit (cell : Cell) (digit : Digit) : Value = 
    { Value.cell = cell
      digit = digit }

let makeCandidate (cell : Cell) (digit : Digit) : Candidate =
    { Candidate.cell = cell
      digit = digit }

type ColumnCells = Lookup<Column, Set<Cell>>

type RowCells = Lookup<Row, Set<Cell>>

type ColumnStack = Lookup<Column, Stack>

type StackColumns = Lookup<Stack, Set<Column>>

type RowBand = Lookup<Row, Band>

type BandRows = Lookup<Band, Set<Row>>

type CellBox = Lookup<Cell, Box>

type BoxCells = Lookup<Box, Set<Cell>>

// for a house, return the cells in it
type HouseCells = Lookup<House, Set<Cell>>

// for a cell, return the cells in the column, row and box it belongs to
type CellHouseCells =Lookup<Cell, Set<Cell>>

let makeMapLookup<'a, 'b when 'a : comparison and 'b : comparison> (as' : Set<'a>) (fn : 'a -> 'b) : MapLookup<'a, 'b> =
    as'
    |> Set.map (fun a -> (a, fn a))
    |> Map.ofSeq
    |> MapLookup<'a, 'b>

let orderedColumns (length : size) : Column list =
    [ 1..(int) length ]
    |> List.map makeColumn

let orderedRows (length : size) : Row list =
    [ 1..(int) length ]
    |> List.map makeRow

let orderedCells (length : size) : Cell list =
    [ for row in (orderedRows length) do
          for column in (orderedColumns length) do
              yield { Cell.col = column
                      row = row } ]

let orderedStacks (length : size) (boxWidth : boxWidth) : Stack list =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack

let orderedBands (length : size) (boxHeight : boxHeight) : Band list =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand

let orderedStackColumns (boxWidth : boxWidth) (stack : Stack) : Column list =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn

let orderedBandRows (boxHeight : boxHeight) (band : Band) : Row list =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight

    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow

let columns (length : size) : Set<Column> =
    [ 1..(int) length ]
    |> List.map makeColumn
    |> Set.ofList

let rows (length : size) : Set<Row> =
    [ 1..(int) length ]
    |> List.map makeRow
    |> Set.ofList

let cells (length : size) : Set<Cell> =
    orderedCells length
    |> Set.ofList

let stacks (length : size) (boxWidth : boxWidth) : Set<Stack> =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack
    |> Set.ofList

let bands (length : size) (boxHeight : boxHeight) : Set<Band> =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand
    |> Set.ofList

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : Set<Box> =
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { Box.stack = stack
                      band = band } ]
    |> Set.ofList

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : Set<House> =
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

let columnCells (length : size) (column : Column) : Set<Cell> =
    rows length
    |> Set.map (fun row -> 
        { col = column
          row = row })

let rowCells (length : size) (row : Row) : Set<Cell> =
    columns length
    |> Set.map (fun column -> 
        { col = column
          row = row })

let columnStack (boxWidth : boxWidth) (column : Column) : Stack =
    match column with
    | CColumn c ->
        1 + ((int) c- 1) / (int) boxWidth
        |> makeStack

let stackColumns (boxWidth : boxWidth) (stack : Stack) : Set<Column> =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn
        |> Set.ofList

let rowBand (boxHeight : boxHeight) (row : Row) : Band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : boxHeight) (band : Band) : Set<Row> =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight
    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow
    |> Set.ofList

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : Cell) : Box =
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { Box.band = band
      stack = stack }

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : Box) : Set<Cell> =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
          for column in stackColumns do
              yield { Cell.col = column
                      row = row } ]
    |> Set.ofList

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : House) : Set<Cell> =
    match house with
    | HColumn c -> columnCells length c
    | HRow r -> rowCells length r
    | HBox b -> boxCells boxWidth boxHeight b

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : Cell) : Set<Cell> =
    let r : Set<Cell> =
        rowCells length cell.row

    let c : Set<Cell> =
        columnCells length cell.col

    let b : Set<Cell> =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)

    [ r; c; b ]
    |> Set.unionMany


type PuzzleMap =
    // The order they are normally written in
    abstract member orderedColumns : Column list
    abstract member orderedRows : Row list
    abstract member orderedCells : Cell list
    abstract member orderedStacks : Stack list
    abstract member orderedBands : Band list

    // for a stack, return the columns in it
    abstract member orderedStackColumns : Stack -> Column list

    // for a band, return the rows in it
    abstract member orderedBandRows : Band -> Row list

    abstract member columns : Set<Column>

    abstract member rows : Set<Row>

    abstract member cells : Set<Cell>

    abstract member stacks : Set<Stack>

    abstract member bands : Set<Band>

    abstract member boxes : Set<Box>

    abstract member houses : Set<House>

    // for a column, return the cells in it
    abstract member columnCells : ColumnCells

    // for a row, return the cells in it
    abstract member rowCells : RowCells

    // for a column, which stack is it in?
    abstract member columnStack : ColumnStack

    // for a stack, return the columns in it
    abstract member stackColumns : StackColumns

    // for a row, which band is it in?
    abstract member rowBand : RowBand

    // for a band, return the rows in it
    abstract member bandRows : BandRows

    // for a cell, which box is it in?
    abstract member cellBox : CellBox

    // for a box, return the cells in it
    abstract member boxCells : BoxCells

    // for a house, return the cells in it
    abstract member houseCells : HouseCells

    abstract member cellHouseCells : CellHouseCells

type TPuzzleMap(puzzleShape : PuzzleShape) =

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
        makeMapLookup<Column, Set<Cell>> _columns (fun column -> columnCells puzzleShape.size column)
        :> ColumnCells

    let _rowCells =
        makeMapLookup<Row, Set<Cell>> _rows (fun row -> rowCells puzzleShape.size row)
        :> RowCells

    let _columnStack =
        makeMapLookup<Column, Stack> _columns (fun column -> columnStack puzzleShape.boxWidth column)
        :> ColumnStack

    let _stackColumns =
        makeMapLookup<Stack, Set<Column>> _stacks (fun stack -> stackColumns puzzleShape.boxWidth stack)
        :> StackColumns

    let _rowBand =
        makeMapLookup<Row, Band> _rows (fun row -> rowBand puzzleShape.boxHeight row)
        :> RowBand

    let _bandRows =
        makeMapLookup<Band, Set<Row>> _bands (fun band -> bandRows puzzleShape.boxHeight band)
        :> BandRows

    let _cellBox =
        makeMapLookup<Cell, Box> _cells (fun cell -> cellBox puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> CellBox

    let _boxCells =
        makeMapLookup<Box, Set<Cell>> _boxes (fun box -> boxCells puzzleShape.boxWidth puzzleShape.boxHeight box)
        :> BoxCells

    let _houseCells =
        makeMapLookup<House, Set<Cell>> _houses (fun house -> houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight house)
        :> HouseCells

    let _cellHouseCells =
        makeMapLookup<Cell, Set<Cell>> _cells (fun cell -> cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> CellHouseCells

    interface PuzzleMap with

        // The order they are normally written in
        member this.orderedColumns : Column list = _orderedColumns
        member this.orderedRows : Row list = _orderedRows
        member this.orderedCells : Cell list = _orderedCells
        member this.orderedStacks : Stack list = _orderedStacks
        member this.orderedBands : Band list = _orderedBands

        // for a stack, return the columns in it
        member this.orderedStackColumns (stack : Stack) : Column list = orderedStackColumns puzzleShape.boxWidth stack

        // for a band, return the rows in it
        member this.orderedBandRows (band : Band) : Row list = orderedBandRows puzzleShape.boxHeight band

        member this.columns : Set<Column> = _columns

        member this.rows : Set<Row> = _rows

        member this.cells : Set<Cell> = _cells

        member this.stacks : Set<Stack> = _stacks

        member this.bands : Set<Band> = _bands

        member this.boxes : Set<Box> = _boxes

        member this.houses : Set<House> = _houses

        // for a column, return the cells in it
        member this.columnCells : ColumnCells = _columnCells

        // for a row, return the cells in it
        member this.rowCells : RowCells = _rowCells

        // for a column, which stack is it in?
        member this.columnStack : ColumnStack = _columnStack

        // for a stack, return the columns in it
        member this.stackColumns : StackColumns = _stackColumns

        // for a row, which band is it in?
        member this.rowBand : RowBand = _rowBand

        // for a band, return the rows in it
        member this.bandRows : BandRows = _bandRows

        // for a cell, which box is it in?
        member this.cellBox : CellBox = _cellBox

        // for a box, return the cells in it
        member this.boxCells : BoxCells = _boxCells

        // for a house, return the cells in it
        member this.houseCells : HouseCells = _houseCells

        member this.cellHouseCells : CellHouseCells = _cellHouseCells
