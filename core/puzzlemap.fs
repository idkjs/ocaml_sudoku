module core.puzzlemap

open sudoku

let makeColumn (i : int) : column =
    i |> CColumn

let makeRow (i : int) : row =
    i |> RRow

let makeCell (c : column) (r : row) : cell =
    { cell.col = c;
      row = r }

let makeStack (i : int) : stack =
    i |> SStack

let makeBand (i : int) : band =
    i |> BBand

let makeBox (s : stack) (b : band) : box =
    { box.stack = s;
      band = b }

let makeValue (cell : cell) (digit : digit) : value = 
    { value.cell = cell
      digit = digit }

let makeCandidate (cell : cell) (digit : digit) : candidate =
    { candidate.cell = cell
      digit = digit }

let makeCandidateReduction (cell : cell) (digits : digits) : candidateReduction =
    { candidateReduction.cell = cell;
      candidates = digits }

let columns (length : size) : column array =
    [| 1..(int) length |]
    |> Array.map makeColumn

let rows (length : size) : row array =
    [| 1..(int) length |]
    |> Array.map makeRow

let cells (length : size) : cell array =
    [| for row in (rows length) do
          for column in (columns length) do
              yield makeCell column row |]

let stacks (length : size) (boxWidth : boxWidth) : stack array =
    [| 1..((int) length / (int) boxWidth) |]
    |> Array.map makeStack

let bands (length : size) (boxHeight : boxHeight) : band array =
    [| 1..((int) length / (int) boxHeight) |]
    |> Array.map makeBand

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : box array =
    [| for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield makeBox stack band |]

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
        (fun row -> makeCell column row)

let rowCells (length : size) (row : row) : cell array =
    columns length
    |> Array.map
        (fun column -> makeCell column row)

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
    makeBox stack band

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : cell array =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [| for row in bandRows do
          for column in stackColumns do
              yield makeCell column row |]

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

[<NoComparisonAttribute;NoEqualityAttribute>]
type puzzleMap =
    {
        columns : column array
        rows : row array
        cells : cell array
        stacks : stack array
        bands : band array
        boxes : box array
        houses : house array
        (* for a column, return the cells in it *)
        columnCells : lookup<column, cell array>
        (* for a row, return the cells in it *)
        rowCells : lookup<row, cell array>
        (* for a column, which stack is it in? *)
        columnStack : lookup<column, stack>
        (* for a stack, return the columns in it *)
        stackColumns : lookup<stack, column array>
        (* for a row, which band is it in? *)
        rowBand : lookup<row, band>
        (* for a band, return the rows in it *)
        bandRows : lookup<band, row array>
        (* for a cell, which box is it in? *)
        cellBox : lookup<cell, box>
        (* for a box, return the cells in it *)
        boxCells : lookup<box, cell array>
        (* for a house, return the cells in it *)
        houseCells : lookup<house, cells>
        cellHouseCells : lookup<cell, cells>
        housesCells : houses -> cells
        houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

        //abstract member houseCellCandidates : lookup<house, cellCandidates>
    }

let tPuzzleMap (puzzleShape : puzzleShape) : puzzleMap =

    let _columns = columns puzzleShape.size
    let _rows = rows puzzleShape.size
    let _cells = cells puzzleShape.size
    let _stacks = stacks puzzleShape.size puzzleShape.boxWidth
    let _bands = bands puzzleShape.size puzzleShape.boxHeight
    let _boxes = boxes puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight
    let _houses = houses puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight
    let _columnCells = columnCells puzzleShape.size
    let _rowCells = rowCells puzzleShape.size
    let _columnStack = columnStack puzzleShape.boxWidth
    let _stackColumns = stackColumns puzzleShape.boxWidth
    let _rowBand = rowBand puzzleShape.boxHeight
    let _bandRows = bandRows puzzleShape.boxHeight
    let _cellBox = cellBox puzzleShape.boxWidth puzzleShape.boxHeight
    let _boxCells = boxCells puzzleShape.boxWidth puzzleShape.boxHeight
    let _houseCells = houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight
    let _cellHouseCells = cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight

    let _columnCellsLookup =
        makeMapLookup<column, cell array> _columns _columnCells
        :> lookup<column, cell array>

    let _rowCellsLookup =
        makeMapLookup<row, cell array> _rows _rowCells
        :> lookup<row, cell array>

    let _columnStackLookup =
        makeMapLookup<column, stack> _columns _columnStack
        :> lookup<column, stack>

    let _stackColumnsLookup =
        makeMapLookup<stack, column array> _stacks _stackColumns
        :> lookup<stack, column array>

    let _rowBandLookup =
        makeMapLookup<row, band> _rows _rowBand
        :> lookup<row, band>

    let _bandRowsLookup =
        makeMapLookup<band, row array> _bands _bandRows
        :> lookup<band, row array>

    let _cellBoxLookup =
        makeMapLookup<cell, box> _cells _cellBox
        :> lookup<cell, box>

    let _boxCellsLookup =
        makeMapLookup<box, cell array> _boxes _boxCells
        :> lookup<box, cell array>

    let _houseCellsLookup =
        makeMapLookup<house, cells> _houses _houseCells
        :> lookup<house, cells>

    let _cellHouseCellsLookup =
        makeMapLookup<cell, cells> _cells _cellHouseCells
        :> lookup<cell, cells>

    let _housesCells (houses : houses) : cells =
        houses
        |> Houses.map _houseCellsLookup.Get
        |> Cells.unionMany

    let _houseCellCandidateReductions (house : house) (cellCandidates : cellCandidates) : candidateReductions =
        _houseCellsLookup.Get house
        |> Cells.map (fun cell -> makeCandidateReduction cell (cellCandidates.Get cell))
        |> CandidateReductions.ofSet

    {
        puzzleMap.columns = _columns
        rows = _rows
        cells = _cells
        stacks = _stacks
        bands = _bands
        boxes = _boxes
        houses = _houses
        columnCells = _columnCellsLookup
        rowCells = _rowCellsLookup
        columnStack = _columnStackLookup
        stackColumns = _stackColumnsLookup
        rowBand = _rowBandLookup
        bandRows = _bandRowsLookup
        cellBox = _cellBoxLookup
        boxCells = _boxCellsLookup
        houseCells = _houseCellsLookup
        cellHouseCells = _cellHouseCellsLookup
        housesCells = _housesCells
        houseCellCandidateReductions = _houseCellCandidateReductions
    }
