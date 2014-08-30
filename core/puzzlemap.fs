module core.puzzlemap

open sudoku
open tactics

let konst x _ = x

let candidateToSymbol (Candidate s : Candidate) = Symbol s

let symbolToCandidate (Symbol s : Symbol) = Candidate s

[<NoEquality; NoComparison>]
type PuzzleMaps = 
    { columns : Column list
      rows : Row list
      cells : Cell list

      cellColumn : Cell -> Column
      columnCells : Column -> Cell list
      cellRow : Cell -> Row
      rowCells : Row -> Cell list
      cellBox : Cell -> Box
      boxCells : Box -> Cell list

      stacks : Stack list
      bands : Band list
      boxes : Box list

      columnStack : Column -> Stack
      stackColumns : Stack -> Column list
      rowBand : Row -> Band
      bandRows : Band -> Row list }

let makePuzzleMaps (puzzleSpec : Puzzle) = 
    let length = puzzleSpec.alphabet.Length

    let columns = columns length
    let rows = rows length
    let cells = allCells length

    let columnCells = columnCells length
    let rowCells = rowCells length

    let (cellBoxLookup, boxCellLookup) = 
        makeContainerBoxCell (boxes puzzleSpec.boxWidth puzzleSpec.boxHeight length) cells puzzleSpec.boxWidth 
            puzzleSpec.boxHeight
    
    let houseCells = 
        { columns = columns
          rows = rows
          cells = cells

          cellColumn = cellColumn
          columnCells = columnCells
          cellRow = cellRow
          rowCells = rowCells
          cellBox = cellBoxLookup
          boxCells = boxCellLookup

          stacks = stacks puzzleSpec.boxWidth length
          bands = bands puzzleSpec.boxHeight length
          boxes = boxes puzzleSpec.boxWidth puzzleSpec.boxHeight length

          columnStack = columnStack puzzleSpec.boxWidth
          stackColumns = stackColumns puzzleSpec.boxWidth
          rowBand = rowBand puzzleSpec.boxHeight
          bandRows = bandRows puzzleSpec.boxHeight }

    houseCells

let getHouseCells (puzzleMaps : PuzzleMaps) (h : House) = 
    match h with
    | Column c -> puzzleMaps.columnCells c |> Set.ofList
    | Row r -> puzzleMaps.rowCells r |> Set.ofList
    | Box b -> puzzleMaps.boxCells b |> Set.ofList

let allHouses (puzzleMaps : PuzzleMaps) = 
    let chs = List.map (fun c -> Column c) puzzleMaps.columns

    let rhs = List.map (fun r -> Row r) puzzleMaps.rows

    let bhs = List.map (fun b -> Box b) puzzleMaps.boxes

    List.concat [ chs; rhs; bhs ]

// For a cell, return all the cell symbols for its containing column
let rowCellsForCell (puzzleMaps : PuzzleMaps) = puzzleMaps.cellRow >> puzzleMaps.rowCells

// For a cell, return all the cell symbols for its containing row
let colCellsForCell (puzzleMaps : PuzzleMaps) = puzzleMaps.cellColumn >> puzzleMaps.columnCells

// For a cell, return all the cell symbols for its containing box
let boxCellsForCell (puzzleMaps : PuzzleMaps) = puzzleMaps.cellBox >> puzzleMaps.boxCells

let allHouseCells (puzzleMaps : PuzzleMaps) cell = 
    let r = 
        cell
        |> rowCellsForCell puzzleMaps
        |> Set.ofList
    
    let c = 
        cell
        |> colCellsForCell puzzleMaps
        |> Set.ofList
    
    let b = 
        cell
        |> boxCellsForCell puzzleMaps
        |> Set.ofList
    
    Set.unionMany [ r; c; b ]

let houseCellsForCell (puzzleMaps : PuzzleMaps) (cell : Cell) (symbolLookup : Cell -> 'a option) = 
    let a = allHouseCells puzzleMaps cell

    let sys = Set.map symbolLookup a
    let sys2 = Set.filter (fun (s : 'a option) -> s.IsSome) sys
    Set.map (fun (s : 'a option) -> s.Value) sys2

let setCellValueModelEffect (puzzleMaps : PuzzleMaps) (setCellValue : SetCellValue) 
    (candidateLookup : Cell -> Set<Candidate>) : Set<CandidateReduction> = 
    let houseCells = allHouseCells puzzleMaps setCellValue.cell
    let otherHouseCells = Set.remove setCellValue.cell houseCells
    
    let candidateReductions = 
        Set.filter (fun c -> 
            let cs = candidateLookup c
            Set.contains setCellValue.candidate cs) otherHouseCells
    
    let candidateReductionCells = 
        Set.map (fun c -> 
            { CandidateReduction.cell = c
              symbols = set [ setCellValue.candidate ] }) candidateReductions
    
    candidateReductionCells

let setCellCandidateReductions (puzzleMaps : PuzzleMaps) (setCellValue : SetCellValue) 
    (candidateLookup : Cell -> Set<Candidate>) : Set<CandidateReduction> = 
    let candidateReductionCells = setCellValueModelEffect puzzleMaps setCellValue candidateLookup

    let ccs = candidateLookup setCellValue.cell
    let ccs2 = Set.remove setCellValue.candidate ccs
    
    let crs = 
        { CandidateReduction.cell = setCellValue.cell
          symbols = ccs2 }
    
    let crs3 = Set.add crs candidateReductionCells

    crs3

