module core.sudoku

// A sudoku is a square grid of size...
[<Measure>]
type size

// containing columns...
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

// The grid is divided into boxes,
// these do not have to be square, but they are
// all the same size and cover the grid
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

[<Measure>]
type height

val makeBand : int -> Band

val bands : int<size> -> int<height> -> Band list

// A box is the intersection of a stack and a band
type Box = 
    { stack : Stack
      band : Band }

val boxes : int<size> -> int<width> -> int<height> -> Box list

// The columns, rows and boxes are collectively called houses
type House = 
    | Column of Column
    | Row of Row
    | Box of Box

val houses : int<size> -> int<width> -> int<height> -> House list

// Each cell in the grid contains a symbol, usually numbers 1..9
type Symbol = 
    | Symbol of char

// these are just the same as symbols, just call them candidates
// when we're still working out which it's to be
type Candidate = 
    | Candidate of char

val symbolToCandidate : Symbol -> Candidate

val candidateToSymbol : Candidate -> Symbol

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the symbols in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type Puzzle = 
    { boxWidth : int<width>
      boxHeight : int<height>
      alphabet : Symbol list }

// Whilst working to a solution each cell in the grid
// that doesn't have a symbol is filled with candidates
// Candidates are possible symbols
[<NoEquality; NoComparison>]
type CellContents = 
    | ASymbol of Symbol
    | ACandidates of Set<Candidate>

// Working towards a solution we take one of the following actions:
// Set the cell to have a symbol
type SetCellSymbol = 
    { cell : Cell
      symbol : Symbol }

// or remove a candidate
type ClearCellCandidate = 
    { cell : Cell
      candidate : Candidate }

type Action = 
    | SetCellSymbol of SetCellSymbol
    | ClearCellCandidate of ClearCellCandidate

[<NoEquality; NoComparison>]
type Solution = 
    { start : Cell -> Symbol option
      current : Cell -> CellContents
      steps : Action list }

// To draw a cell we may want to display extra information...
[<NoEquality; NoComparison>]
type CellAnnotation = 
    { setValue : Symbol option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : Candidate option
      reductions : Set<Candidate>
      pointers : Set<Candidate> }
