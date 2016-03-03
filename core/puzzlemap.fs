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

let orderedColumns (length : size) : column array =
    [| 1..(int) length |]
    |> Array.map makeColumn

let orderedRows (length : size) : row array =
    [| 1..(int) length |]
    |> Array.map makeRow

let orderedCells (length : size) : cell array =
    [| for row in (orderedRows length) do
          for column in (orderedColumns length) do
              yield { cell.col = column
                      row = row } |]

let orderedStacks (length : size) (boxWidth : boxWidth) : stack array =
    [| 1..((int) length / (int) boxWidth) |]
    |> Array.map makeStack

let orderedBands (length : size) (boxHeight : boxHeight) : band array =
    [| 1..((int) length / (int) boxHeight) |]
    |> Array.map makeBand

let orderedStackColumns (boxWidth : boxWidth) (stack : stack) : column array =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [| (t + 1)..(t + (int) boxWidth) |]
        |> Array.map makeColumn

let orderedBandRows (boxHeight : boxHeight) (band : band) : row array =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight

    [| (c + 1)..(c + (int) boxHeight) |]
    |> Array.map makeRow

let columns (length : size) : column array =
    [| 1..(int) length |]
    |> Array.map makeColumn

let rows (length : size) : row array =
    [| 1..(int) length |]
    |> Array.map makeRow

let cells (length : size) : cell array =
    orderedCells length

let stacks (length : size) (boxWidth : boxWidth) : stack array =
    [| 1..((int) length / (int) boxWidth) |]
    |> Array.map makeStack

let bands (length : size) (boxHeight : boxHeight) : band array =
    [| 1..((int) length / (int) boxHeight) |]
    |> Array.map makeBand

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : box array =
    [| for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { box.stack = stack
                      band = band } |]

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : house array =
    let chs =
        columns length
        |> Array.map HColumn

    let rhs =
        rows length
        |> Array.map HRow

    let bhs =
        boxes length boxWidth boxHeight
        |> Array.map HBox

    [ chs; rhs; bhs ]
    |> Array.concat

let columnCells (length : size) (column : column) : cell array =
    rows length
    |> Array.map
        (fun row -> 
            { col = column
              row = row })

let rowCells (length : size) (row : row) : cell array =
    columns length
    |> Array.map
        (fun column -> 
            { col = column
              row = row })

let columnStack (boxWidth : boxWidth) (column : column) : stack =
    match column with
    | CColumn c ->
        1 + ((int) c- 1) / (int) boxWidth
        |> makeStack

let stackColumns (boxWidth : boxWidth) (stack : stack) : column array =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [| (t + 1)..(t + (int) boxWidth) |]
        |> Array.map makeColumn

let rowBand (boxHeight : boxHeight) (row : row) : band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : boxHeight) (band : band) : row array =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight
    [| (c + 1)..(c + (int) boxHeight) |]
    |> Array.map makeRow

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : box =
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    { box.band = band
      stack = stack }

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : cell array =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [| for row in bandRows do
          for column in stackColumns do
              yield { cell.col = column
                      row = row } |]

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : house) : cells =
    match house with
    | HColumn c -> columnCells length c |> Cells.ofArray
    | HRow r -> rowCells length r |> Cells.ofArray
    | HBox b -> boxCells boxWidth boxHeight b |> Cells.ofArray

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : cells =
    let rowCells =
        rowCells length cell.row
        |> Cells.ofArray
        |> Cells.remove cell

    let columnCells =
        columnCells length cell.col
        |> Cells.ofArray
        |> Cells.remove cell

    let boxCells =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)
        |> Cells.ofArray
        |> Cells.remove cell

    [| Cells.singleton cell; rowCells; columnCells; boxCells |]
    |> Cells.unionMany

type puzzleMap =

    abstract member columns : column array

    abstract member rows : row array

    abstract member cells : cell array

    abstract member stacks : stack array

    abstract member bands : band array

    abstract member boxes : box array

    abstract member houses : house array

    (* for a column, return the cells in it *)
    abstract member columnCells : lookup<column, cell array>

    (* for a row, return the cells in it *)
    abstract member rowCells : lookup<row, cell array>

    (* for a column, which stack is it in? *)
    abstract member columnStack : lookup<column, stack>

    (* for a stack, return the columns in it *)
    abstract member stackColumns : lookup<stack, column array>

    (* for a row, which band is it in? *)
    abstract member rowBand : lookup<row, band>

    (* for a band, return the rows in it *)
    abstract member bandRows : lookup<band, row array>

    (* for a cell, which box is it in? *)
    abstract member cellBox : lookup<cell, box>

    (* for a box, return the cells in it *)
    abstract member boxCells : lookup<box, cell array>

    (* for a house, return the cells in it *)
    abstract member houseCells : lookup<house, cells>

    abstract member cellHouseCells : lookup<cell, cells>

    abstract member housesCells : houses -> cells

    abstract member houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

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
        makeMapLookup<column, cell array> _columns (fun column -> columnCells puzzleShape.size column)
        :> lookup<column, cell array>

    let _rowCells =
        makeMapLookup<row, cell array> _rows (fun row -> rowCells puzzleShape.size row)
        :> lookup<row, cell array>

    let _columnStack =
        makeMapLookup<column, stack> _columns (fun column -> columnStack puzzleShape.boxWidth column)
        :> lookup<column, stack>

    let _stackColumns =
        makeMapLookup<stack, column array> _stacks (fun stack -> stackColumns puzzleShape.boxWidth stack)
        :> lookup<stack, column array>

    let _rowBand =
        makeMapLookup<row, band> _rows (fun row -> rowBand puzzleShape.boxHeight row)
        :> lookup<row, band>

    let _bandRows =
        makeMapLookup<band, row array> _bands (fun band -> bandRows puzzleShape.boxHeight band)
        :> lookup<band, row array>

    let _cellBox =
        makeMapLookup<cell, box> _cells (fun cell -> cellBox puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> lookup<cell, box>

    let _boxCells =
        makeMapLookup<box, cell array> _boxes (fun box -> boxCells puzzleShape.boxWidth puzzleShape.boxHeight box)
        :> lookup<box, cell array>

    let _houseCells =
        makeMapLookup<house, cells> _houses (fun house -> houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight house)
        :> lookup<house, cells>

    let _cellHouseCells =
        makeMapLookup<cell, cells> _cells (fun cell -> cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight cell)
        :> lookup<cell, cells>

    interface puzzleMap with

        member this.columns : column array = _columns

        member this.rows : row array = _rows

        member this.cells : cell array = _cells

        member this.stacks : stack array = _stacks

        member this.bands : band array = _bands

        member this.boxes : box array = _boxes

        member this.houses : house array = _houses

        (* for a column, return the cells in it *)
        member this.columnCells : lookup<column, cell array> = _columnCells

        (* for a row, return the cells in it *)
        member this.rowCells : lookup<row, cell array> = _rowCells

        (* for a column, which stack is it in? *)
        member this.columnStack : lookup<column, stack> = _columnStack

        (* for a stack, return the columns in it *)
        member this.stackColumns : lookup<stack, column array> = _stackColumns

        (* for a row, which band is it in? *)
        member this.rowBand : lookup<row, band> = _rowBand

        (* for a band, return the rows in it *)
        member this.bandRows : lookup<band, row array> = _bandRows

        (* for a cell, which box is it in? *)
        member this.cellBox : lookup<cell, box> = _cellBox

        (* for a box, return the cells in it *)
        member this.boxCells : lookup<box, cell array> = _boxCells

        (* for a house, return the cells in it *)
        member this.houseCells : lookup<house, cells> = _houseCells

        member this.cellHouseCells : lookup<cell, cells> = _cellHouseCells

        member this.housesCells (houses : houses) : cells =
            houses
            |> Houses.map _houseCells.Get
            |> Cells.unionMany

        member this.houseCellCandidateReductions (house : house) (cellCandidates : cellCandidates) : candidateReductions =
            _houseCells.Get house
            |> Cells.map (fun cell -> { candidateReduction.cell = cell; candidates = cellCandidates.Get cell })
            |> CandidateReductions.ofSet
