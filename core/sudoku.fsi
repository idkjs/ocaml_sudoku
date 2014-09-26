module core.sudoku

// A sudoku is a grid of columns...
[<Measure>]
type size

[<Measure>]
type column

type Column = 
    { col : int<column> }

val makeColumn : int -> Column

val columns : int<size> -> Column list

// ... by rows
[<Measure>]
type row

type Row = 
    { row : int<row> }

val makeRow : int -> Row

val rows : int<size> -> Row list

// Each cell is identified by (col, row)
type Cell = 
    { col : Column
      row : Row }

val cells : int<size> -> Cell list

// The grid is divided into boxes
[<Measure>]
type boxcol

// A column of vertical boxes is a stack
type Stack = 
    { stack : int<boxcol> }

[<Measure>]
type width

val makeStack : int -> Stack

val stacks : int<size> -> int<width> -> Stack list

[<Measure>]
type boxrow

// A row of horizontal boxes is a band
type Band = 
    { band : int<boxrow> }

val makeBand : int -> Band

[<Measure>]
type height

val bands : int<size> -> int<height> -> Band list

// A box is the intersection of a stack and a band
type Box = 
    { stack : Stack
      band : Band }

val boxes : int<size> -> int<width> -> int<height> -> Box list

// The columns, rows and boxes are called houses
type House = 
    | Column of Column
    | Row of Row
    | Box of Box

val houses : int<size> -> int<width> -> int<height> -> House list

// Each cell in the grid contains a symbol, usually numbers 1..9
type Symbol = 
    | Symbol of char

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the symbols in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type Puzzle = 
    { boxWidth : int<width>
      boxHeight : int<height>
      alphabet : Symbol list
      size : int<size>
      symbols : Cell -> Symbol option }

type Candidate = 
    | Candidate of char

// Whilst working to a solution each cell in the grid
// that doesn't have a symbol is filled with candidates
[<NoEquality; NoComparison>]
type CellContents = 
    | ASymbol of Symbol
    | ACandidates of Set<Candidate>

[<NoEquality; NoComparison>]
type CellAnnotation = 
    { setValue : Candidate option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      reductions : Set<Candidate>
      pointers : Set<Candidate> }

// Working towards a solution we take one of the following actions:
type SetCellValue = 
    { cell : Cell
      reductions : Set<Cell>
      candidate : Candidate }

type ClearCandidate = 
    { cell : Cell
      candidate : Candidate }

type Action = 
    | SetCellValue of SetCellValue
    | ClearCandidate of ClearCandidate

[<NoEquality; NoComparison>]
type Solution = 
    { start : Cell -> Symbol option
      current : Cell -> CellContents
      steps : Action list }

type CandidateReduction = 
    { cell : Cell
      candidates : Set<Candidate> }
