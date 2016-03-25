open Smap
open Sudoku

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
    { value.cell = cell;
      digit = digit }

let makeCandidate (cell : cell) (digit : digit) : candidate =
    { candidate.cell = cell;
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
    let columns' = columns length in

    rows length
    |> List.map
        (fun row ->
            columns'
            |> List.map
                (fun column -> makeCell column row))
    |> List.concat

let stacks (length : size) (boxWidth : boxWidth) : stack list =
    [ 1..((int) length / (int) boxWidth) ]
    |> List.map makeStack

let bands (length : size) (boxHeight : boxHeight) : band list =
    [ 1..((int) length / (int) boxHeight) ]
    |> List.map makeBand

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : box list =
    let stacks' = stacks length boxWidth in

    bands length boxHeight
    |> List.map
        (fun band ->
          stacks'
          |> List.map (fun stack -> makeBox stack band))
    |> List.concat

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : house list =
    let chs = List.map HColumn (columns length) in
    let rhs = List.map HRow (rows length) in
    let bhs = List.map HBox (boxes length boxWidth boxHeight) in

    List.concat [ chs; rhs; bhs ]

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
        let t = ((int) s - 1) * (int) boxWidth in
        [ (t + 1)..(t + (int) boxWidth) ]
        |> List.map makeColumn

let rowBand (boxHeight : boxHeight) (row : row) : band =
    match row with
    | RRow r ->
        1 + ((int) r - 1) / (int) boxHeight
        |> makeBand

let bandRows (boxHeight : boxHeight) (band : band) : row list =
    let c = match band with BBand b -> ((int) b - 1) * (int) boxHeight in

    [ (c + 1)..(c + (int) boxHeight) ]
    |> List.map makeRow

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : box =
    let stack = columnStack boxWidth cell.col in
    let band = rowBand boxHeight cell.row in
    makeBox stack band

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : cell list =
    let stackColumns = stackColumns boxWidth box.stack in
    let bandRows = bandRows boxHeight box.band in

    bandRows
    |> List.map
        (fun row ->
            stackColumns
            |> List.map (fun column -> makeCell column row))
    |> List.concat

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : house) : cells =
    match house with
    | HColumn c -> columnCells length c |> Cells.ofList
    | HRow r -> rowCells length r |> Cells.ofList
    | HBox b -> boxCells boxWidth boxHeight b |> Cells.ofList

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : cells =
    let rowCells' =
        rowCells length cell.row
        |> Cells.ofList
        |> Cells.remove cell in

    let columnCells' =
        columnCells length cell.col
        |> Cells.ofList
        |> Cells.remove cell in

    let boxCells' =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)
        |> Cells.ofList
        |> Cells.remove cell in

    [ Cells.singleton cell; rowCells'; columnCells'; boxCells' ]
    |> Cells.unionManyList

[<NoComparisonAttribute;NoEqualityAttribute>]
type puzzleMap =
    {
        columns : columns;
        rows : rows;
        cells : cells;
        stacks : stack list;
        bands : band list;
        boxes : box list;
        houses : house list;
        (* for a column, return the cells in it *)
        columnCells : SMap<column, cells>;
        (* for a row, return the cells in it *)
        rowCells : SMap<row, cells>;
        (* for a column, which stack is it in? *)
        columnStack : SMap<column, stack>;
        (* for a stack, return the columns in it *)
        stackColumns : SMap<stack, column list>;
        (* for a row, which band is it in? *)
        rowBand : SMap<row, band>;
        (* for a band, return the rows in it *)
        bandRows : SMap<band, row list>;
        (* for a cell, which box is it in? *)
        cellBox : SMap<cell, box>;
        (* for a box, return the cells in it *)
        boxCells : SMap<box, cells>;
        (* for a house, return the cells in it *)
        houseCells : SMap<house, cells>;
        cellHouseCells : SMap<cell, cells>;
        housesCells : houses -> cells;
        houseCellCandidateReductions : house -> cellCandidates -> candidateReductions;

        //abstract member houseCellCandidates : SMap<house, cellCandidates>
    }

let tPuzzleMap (puzzleShape : puzzleShape) : puzzleMap =

    let _columns = columns puzzleShape.size in
    let _rows = rows puzzleShape.size in
    let _cells = cells puzzleShape.size in
    let _stacks = stacks puzzleShape.size puzzleShape.boxWidth in
    let _bands = bands puzzleShape.size puzzleShape.boxHeight in
    let _boxes = boxes puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _houses = houses puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _columnCells = columnCells puzzleShape.size >> Cells.ofList in
    let _rowCells = rowCells puzzleShape.size >> Cells.ofList in
    let _columnStack = columnStack puzzleShape.boxWidth in
    let _stackColumns = stackColumns puzzleShape.boxWidth in
    let _rowBand = rowBand puzzleShape.boxHeight in
    let _bandRows = bandRows puzzleShape.boxHeight in
    let _cellBox = cellBox puzzleShape.boxWidth puzzleShape.boxHeight in
    let _boxCells = boxCells puzzleShape.boxWidth puzzleShape.boxHeight >> Cells.ofList in
    let _houseCells = houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _cellHouseCells = cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in

    let _columnCellsLookup = SMap.ofLookup<column, cells> _columns _columnCells in
    let _rowCellsLookup = SMap.ofLookup<row, cells> _rows _rowCells in
    let _columnStackLookup = SMap.ofLookup<column, stack> _columns _columnStack in
    let _stackColumnsLookup = SMap.ofLookup<stack, column list> _stacks _stackColumns in
    let _rowBandLookup = SMap.ofLookup<row, band> _rows _rowBand in
    let _bandRowsLookup = SMap.ofLookup<band, row list> _bands _bandRows in
    let _cellBoxLookup = SMap.ofLookup<cell, box> _cells _cellBox in
    let _boxCellsLookup = SMap.ofLookup<box, cells> _boxes _boxCells in
    let _houseCellsLookup = SMap.ofLookup<house, cells> _houses _houseCells in
    let _cellHouseCellsLookup = SMap.ofLookup<cell, cells> _cells _cellHouseCells in

    let _housesCells (houses : houses) : cells =
        houses
        |> Houses.map (SMap.get _houseCellsLookup)
        |> Cells.unionManyList in

    let _houseCellCandidateReductions (house : house) (cellCandidates : cellCandidates) : candidateReductions =
        SMap.get _houseCellsLookup house
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofList in

    {
        puzzleMap.columns = _columns |> Columns.ofList;
        rows = _rows |> Rows.ofList ;
        cells = _cells |> Cells.ofList;
        stacks = _stacks;
        bands = _bands;
        boxes = _boxes;
        houses = _houses;
        columnCells = _columnCellsLookup;
        rowCells = _rowCellsLookup;
        columnStack = _columnStackLookup;
        stackColumns = _stackColumnsLookup;
        rowBand = _rowBandLookup;
        bandRows = _bandRowsLookup;
        cellBox = _cellBoxLookup;
        boxCells = _boxCellsLookup;
        houseCells = _houseCellsLookup;
        cellHouseCells = _cellHouseCellsLookup;
        housesCells = _housesCells;
        houseCellCandidateReductions = _houseCellCandidateReductions;
    }
