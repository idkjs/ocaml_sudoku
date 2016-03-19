module core.puzzlemap

open sset
open smap
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

let columns (length : size) : column list =
    [ 1..(int) length ]
    |> List.map makeColumn

let rows (length : size) : row list =
    [ 1..(int) length ]
    |> List.map makeRow

let cells (length : size) : cell list =
    [ for row in (rows length) do
          for column in (columns length) do
              yield makeCell column row ]

let stacks (length : size) (boxWidth : boxWidth) : stack list =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack

let bands (length : size) (boxHeight : boxHeight) : band list =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : box list =
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield makeBox stack band ]

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : house list =
    let chs =
        columns length
        |> List.map HColumn

    let rhs =
        rows length
        |> List.map HRow

    let bhs =
        boxes length boxWidth boxHeight
        |> List.map HBox

    [ chs; rhs; bhs ]
    |> List.concat

let columnCells (length : size) (column : column) : cell list =
    rows length
    |> List.map
        (fun row -> makeCell column row)

let rowCells (length : size) (row : row) : cell list =
    columns length
    |> List.map
        (fun column -> makeCell column row)

let columnStack (boxWidth : boxWidth) (column : column) : stack =
    match column with
    | CColumn c ->
        1 + ((int) c- 1) / (int) boxWidth
        |> makeStack

let stackColumns (boxWidth : boxWidth) (stack : stack) : column list =
    match stack with
    | SStack s ->
        let t = ((int) s - 1) * (int) boxWidth
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn

let rowBand (boxHeight : boxHeight) (row : row) : band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : boxHeight) (band : band) : row list =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight

    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : box =
    let stack = columnStack boxWidth cell.col
    let band = rowBand boxHeight cell.row
    makeBox stack band

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : cell list =
    let stackColumns = stackColumns boxWidth box.stack
    let bandRows = bandRows boxHeight box.band

    [ for row in bandRows do
          for column in stackColumns do
              yield makeCell column row ]

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : house) : cells =
    match house with
    | HColumn c -> columnCells length c |> Cells.ofList
    | HRow r -> rowCells length r |> Cells.ofList
    | HBox b -> boxCells boxWidth boxHeight b |> Cells.ofList

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : cells =
    let rowCells =
        rowCells length cell.row
        |> Cells.ofList
        |> Cells.remove cell

    let columnCells =
        columnCells length cell.col
        |> Cells.ofList
        |> Cells.remove cell

    let boxCells =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)
        |> Cells.ofList
        |> Cells.remove cell

    [ Cells.singleton cell; rowCells; columnCells; boxCells ]
    |> Cells.unionManyList

[<NoComparisonAttribute;NoEqualityAttribute>]
type puzzleMap =
    {
        columns : column list
        rows : row list
        cells : cell list
        stacks : stack list
        bands : band list
        boxes : box list
        houses : house list
        (* for a column, return the cells in it *)
        columnCells : SMap<column, cell list>
        (* for a row, return the cells in it *)
        rowCells : SMap<row, cell list>
        (* for a column, which stack is it in? *)
        columnStack : SMap<column, stack>
        (* for a stack, return the columns in it *)
        stackColumns : SMap<stack, column list>
        (* for a row, which band is it in? *)
        rowBand : SMap<row, band>
        (* for a band, return the rows in it *)
        bandRows : SMap<band, row list>
        (* for a cell, which box is it in? *)
        cellBox : SMap<cell, box>
        (* for a box, return the cells in it *)
        boxCells : SMap<box, cell list>
        (* for a house, return the cells in it *)
        houseCells : SMap<house, cells>
        cellHouseCells : SMap<cell, cells>
        housesCells : houses -> cells
        houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

        //abstract member houseCellCandidates : SMap<house, cellCandidates>
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
        SMap.ofLookup<column, cell list> _columns _columnCells

    let _rowCellsLookup =
        SMap.ofLookup<row, cell list> _rows _rowCells

    let _columnStackLookup =
        SMap.ofLookup<column, stack> _columns _columnStack

    let _stackColumnsLookup =
        SMap.ofLookup<stack, column list> _stacks _stackColumns

    let _rowBandLookup =
        SMap.ofLookup<row, band> _rows _rowBand

    let _bandRowsLookup =
        SMap.ofLookup<band, row list> _bands _bandRows

    let _cellBoxLookup =
        SMap.ofLookup<cell, box> _cells _cellBox

    let _boxCellsLookup =
        SMap.ofLookup<box, cell list> _boxes _boxCells

    let _houseCellsLookup =
        SMap.ofLookup<house, cells> _houses _houseCells

    let _cellHouseCellsLookup =
        SMap.ofLookup<cell, cells> _cells _cellHouseCells

    let _housesCells (houses : houses) : cells =
        houses
        |> Houses.map (SMap.get _houseCellsLookup)
        |> Cells.unionMany

    let _houseCellCandidateReductions (house : house) (cellCandidates : cellCandidates) : candidateReductions =
        SMap.get _houseCellsLookup house
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
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
