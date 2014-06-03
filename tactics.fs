#light

module tactics

open sudoku

let cons x y = x :: y

type SolutionGrid = System.Collections.Generic.Dictionary<Cell, Entry>

let solutionGridCellLookup (grid:SolutionGrid) = fun cell -> grid.[cell]

type Container<'a, 'b> = ('a * 'b) list

[<NoEquality; NoComparison>]
type HouseCells = {
    cellColumn : (Cell -> Column)
    columnCells : (Column -> Cell list)
    cellRow : (Cell -> Row)
    rowCells : (Row -> Cell list)
    cellBox : (Cell -> Box)
    boxCells : (Box -> Cell list)
}

let getHouseCells (houseCells:HouseCells) (h:House) =
    match h with
    | Column c -> houseCells.columnCells c
    | Row r -> houseCells.rowCells r
    | Box b -> houseCells.boxCells b

let contentsOfContainer<'a, 'b when 'b : equality> (s : 'b) (sc : Container<'b, 'a>) = List.filter (fun p -> s = fst p) sc |> List.map (fun p -> snd p)

let containerOfContent<'a, 'b when 'a : equality> (sc : Container<'b, 'a>) (c : 'a) = List.find (fun p -> c = snd p) sc |> fst

let eachContainerSet<'a, 'b when 'a:equality and 'b:equality> (makeElement : int -> 'a) (makeContainer : int -> 'b) boxSize gridSize =

    let elements (c:int) = [ 
        for elementIndex in c .. c + boxSize - 1 do
            let element = makeElement elementIndex
            yield element ]

    let all = [
        for i in 1 .. boxSize .. gridSize do
            let containerIndex = (((i - 1) / boxSize) + 1)
            let c = makeContainer containerIndex
            let es = elements i
            let ces = List.map (fun e -> (c, e)) es
            yield (c, es, ces) ]

    let containers = List.map (fun (a, _, _) -> a) all
    let elements = List.collect (fun (_, b, _) -> b) all
    let elementContainers = List.collect (fun (_, _, c) -> c) all

    let elementContainerLookup = (fun c -> containerOfContent elementContainers c)
    let containerElementLookup = (fun s -> contentsOfContainer s elementContainers)

    (containers, elements, elementContainerLookup, containerElementLookup)

let eachStackColumns boxWidth gridWidth =
    eachContainerSet<Column, Stack>
        (fun i -> { Column.col = i * 1<col> }) 
        (fun i -> { Stack.stack = i * 1<boxcol> })
        boxWidth
        gridWidth

let eachBandRows boxHeight gridHeight =
    eachContainerSet<Row, Band>
        (fun i -> { Row.row = i * 1<row> }) 
        (fun i -> { Band.band = i * 1<boxrow> })
        boxHeight
        gridHeight

let eachBox (stacks:Stack list) (bands:Band list) = 
    let all = [
        for band in bands do
            for stack in stacks do
                yield { Box.stack = stack; band = band } ]

    all

let allCells (columns:Column list) (rows:Row list)= [
    for row in rows do
        for column in columns do
            yield { Cell.col = column; row = row } ]

let makeContainerContainerCell<'a when 'a:equality> (columns:'a list) (allCells:Cell list) (container: Cell -> 'a) = 
    let cc = List.map (fun cell -> (container cell, cell)) allCells
    let ccLookup = (fun b -> contentsOfContainer b cc)
    (cc, ccLookup)

let makeContainerColumnCell (columns:Column list) (cells:Cell list) =
    let (columnCells, columnCellLookup) = makeContainerContainerCell<Column> columns cells (fun cell -> cell.col)
    let cellColumnLookup = (fun c -> c.col)
    (columnCellLookup, cellColumnLookup)

let makeContainerRowCell (rows:Row list) (cells:Cell list) =
    let (rowCells, rowCellLookup) = makeContainerContainerCell<Row> rows cells (fun cell -> cell.row)
    let cellRowLookup = (fun c -> c.row)
    (rowCellLookup, cellRowLookup)

let makeContainerBoxCell (boxes:Box list) (cells:Cell list) (columnStack:Column -> Stack) (rowBand:Row -> Band) =
    let (boxCells, boxCellLookup) = 
        makeContainerContainerCell<Box> boxes cells
            (fun cell ->
                let col = cell.col
                let stack = columnStack col
                let row = cell.row
                let band = rowBand row
                { Box.band = band; stack = stack })
    let cellBoxLookup = (fun c -> containerOfContent boxCells c)
    (cellBoxLookup, boxCellLookup)

// and v.v.
let charToAlphabet (alphabet : Alphabet) (trialSymbol : char) = 
  let compareAlpha symbol =
    let (Symbol charSymbol) = symbol
    trialSymbol = charSymbol

  List.tryFind compareAlpha alphabet

let entryToAlphabet = function
    | Given(g) -> Some(g)
    | Set(s) -> Some(s)
    | Candidates(_) -> None

// For a cell, return all the cell symbols for its containing column
let rowCellsForCell (c2c:Cell -> Column) (cc:Column -> Cell list) = c2c >> cc

// For a cell, return all the cell symbols for its containing row
let colCellsForCell (cr:Cell -> Row) (rc:Row -> Cell list) = cr >> rc

// For a cell, return all the cell symbols for its containing box
let boxCellsForCell (cb:Cell -> Box) (bc:Box -> Cell list) = cb >> bc

let allHouseCells cell (houseCells:HouseCells) =
  let r = cell |> rowCellsForCell houseCells.cellColumn houseCells.columnCells
  let c = cell |> colCellsForCell houseCells.cellRow houseCells.rowCells
  let b = cell |> boxCellsForCell houseCells.cellBox houseCells.boxCells

  let rc = Set.ofList r
  let cc = Set.ofList c
  let bc = Set.ofList b

  Set.unionMany [rc; cc; bc]

let houseCellsForCell symbolLookup cell (houseCells:HouseCells) =
  let r = cell |> rowCellsForCell houseCells.cellColumn houseCells.columnCells |> List.choose symbolLookup
  let c = cell |> colCellsForCell houseCells.cellRow houseCells.rowCells |> List.choose symbolLookup
  let b = cell |> boxCellsForCell houseCells.cellBox houseCells.boxCells |> List.choose symbolLookup

  let rc = Set.ofList r
  let cc = Set.ofList c
  let bc = Set.ofList b

  Set.unionMany [rc; cc; bc]

let candidateSymbols cell symbolLookup (houseCells:HouseCells) existingCandidates =
    Set.difference existingCandidates (houseCellsForCell symbolLookup cell houseCells)


let findCell (c:int<col>) (r:int<row>) (cells:Cell list) =
    List.tryFind (
        fun cell -> cell.col.col = c && cell.row.row = r
    ) cells


let getCandidateEntries = function
    | Given(_) -> Set.empty
    | Set(_) -> Set.empty
    | Candidates(s) -> s

