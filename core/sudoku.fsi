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

// Each cell in the grid contains a Digit, usually numbers 1..9
type Digit = 
    | Digit of char

// these are just the same as Digits, just call them candidates
// when we're still working out which it's to be
type Candidate = 
    | Candidate of char

val digitToCandidate : Digit -> Candidate

val candidateToDigit : Candidate -> Digit

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the Digits in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type Puzzle = 
    { boxWidth : int<width>
      boxHeight : int<height>
      alphabet : Digit list }

// Whilst working to a solution each cell in the grid
// that doesn't have a Digit is filled with candidates
// Candidates are possible Digits
[<NoEquality; NoComparison>]
type CellContents = 
    | ADigit of Digit
    | ACandidates of Set<Candidate>

// Working towards a solution we take one of the following actions:
// Set the cell to have a Digit
type SetCellDigitAction = 
    { cell : Cell
      digit : Digit }

// or remove a candidate
type EliminateCandidateAction = 
    { cell : Cell
      candidate : Candidate }

type Action = 
    | SetCellDigit of SetCellDigitAction
    | EliminateCandidate of EliminateCandidateAction

[<NoEquality; NoComparison>]
type Solution = 
    { given : Cell -> Digit option
      current : Cell -> CellContents
      steps : Action list }

// To draw a cell we may want to display extra information...
[<NoEquality; NoComparison>]
type CellAnnotation = 
    { setValue : Digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : Candidate option
      reductions : Set<Candidate>
      pointers : Set<Candidate> }

// From http://www.fssnip.net/ji
type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b
