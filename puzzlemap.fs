module puzzlemap

open sudoku

let konst x _ = x

let candidateToSymbol (Candidate s:Candidate) = Symbol s

let symbolToCandidate (Symbol s:Symbol) = Candidate s

let getCandidateEntries (alphaset:Set<Candidate>) = function
    | Set _ -> Set.empty
    | Given _ -> Set.empty
    | Candidates s -> Set.filter (fun symbol -> s symbol = Possible) alphaset

let getHouseCells (puzzleMaps:PuzzleMaps) (h:House) =
    match h with
    | Column c -> puzzleMaps.columnCells c
    | Row r -> puzzleMaps.rowCells r
    | Box b -> puzzleMaps.boxCells b


let allHouses (puzzleMaps:PuzzleMaps) =
    let chs =
        List.map
            (fun c -> Column c)
            puzzleMaps.columns

    let rhs =
        List.map
            (fun r -> Row r)
            puzzleMaps.rows

    let bhs =
        List.map
            (fun b -> Box b)
            puzzleMaps.boxes

    List.concat [ chs; rhs; bhs ]


// For a cell, return all the cell symbols for its containing column
let rowCellsForCell (puzzleMaps:PuzzleMaps) = puzzleMaps.cellRow >> puzzleMaps.rowCells

// For a cell, return all the cell symbols for its containing row
let colCellsForCell (puzzleMaps:PuzzleMaps) = puzzleMaps.cellColumn >> puzzleMaps.columnCells

// For a cell, return all the cell symbols for its containing box
let boxCellsForCell (puzzleMaps:PuzzleMaps) = puzzleMaps.cellBox >> puzzleMaps.boxCells

let allHouseCells (puzzleMaps:PuzzleMaps) cell =
  let r = cell |> rowCellsForCell puzzleMaps
  let c = cell |> colCellsForCell puzzleMaps
  let b = cell |> boxCellsForCell puzzleMaps

  let rc = Set.ofList r
  let cc = Set.ofList c
  let bc = Set.ofList b

  Set.unionMany [rc; cc; bc]


let houseCellsForCell (puzzleMaps:PuzzleMaps) (cell:Cell) (symbolLookup:Cell->'a option) =
  let r = cell |> rowCellsForCell puzzleMaps |> List.choose symbolLookup
  let c = cell |> colCellsForCell puzzleMaps |> List.choose symbolLookup
  let b = cell |> boxCellsForCell puzzleMaps |> List.choose symbolLookup

  let rc = Set.ofList r
  let cc = Set.ofList c
  let bc = Set.ofList b

  Set.unionMany [rc; cc; bc]

let flattenEntry (cellLookup:Cell->'a) (cells:Cell list) =
    let s = 
        List.map (
            fun cell -> (cell, cellLookup cell)
        ) cells

    let s2 = s |> Map.ofList

    let solutionGrid = new System.Collections.Generic.Dictionary<Cell, 'a>(s2)

    fun cell -> solutionGrid.[cell]
