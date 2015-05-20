module core.sudoku

open System

// A sudoku is a square grid of size...
[<Measure>]
type size

// containing columns...
[<Measure>]
type column

type Column = 
    { col : int<column> }
    override this.ToString() = String.Format("c{0}", this.col)

let makeColumn i = { Column.col = i * 1<column> }

let columns (length : int<size>) : Column list = List.map makeColumn [ 1..(int) length ]

// ... by rows
[<Measure>]
type row

type Row = 
    { row : int<row> }
    override this.ToString() = String.Format("r{0}", this.row)

let makeRow i = { Row.row = i * 1<row> }

let rows (length : int<size>) = List.map makeRow [ 1..(int) length ]

// Each cell is identified by (col, row)
type Cell = 
    { col : Column
      row : Row }
    override this.ToString() = String.Format("r{0}c{1}", (int) this.row.row, (int) this.col.col)

let cells (length : int<size>) = 
    [ for row in (rows length) do
          for column in (columns length) do
              yield { Cell.col = column
                      row = row } ]

// The grid is divided into boxes,
// these do not have to be square, but they are
// all the same size and cover the grid
[<Measure>]
type stack

// A column of vertical boxes is a stack
type Stack = 
    { stack : int<stack> }
    override this.ToString() = String.Format("stk{0}", (int) this.stack)

[<Measure>]
type boxWidth

let makeStack i = { Stack.stack = i * 1<stack> }

let stacks (length : int<size>) (boxWidth : int<boxWidth>) = List.map makeStack [ 1..((int) length / (int) boxWidth) ]

[<Measure>]
type band

// A row of horizontal boxes is a band
type Band = 
    { band : int<band> }
    override this.ToString() = String.Format("bnd{0}", (int) this.band)

[<Measure>]
type boxHeight

let makeBand i = { Band.band = i * 1<band> }

let bands (length : int<size>) (boxHeight : int<boxHeight>) = List.map makeBand [ 1..((int) length / (int) boxHeight) ]

// A box is the intersection of a stack and a band
type Box = 
    { stack : Stack
      band : Band }
    override this.ToString() = String.Format("stk{0}bnd{1}", (int) this.stack.stack, (int) this.band.band)

let boxes (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) = 
    [ for band in bands length boxHeight do
          for stack in stacks length boxWidth do
              yield { Box.stack = stack
                      band = band } ]

// The columns and rows are collectively called lines
type Line = 
    | LColumn of Column
    | LRow of Row

// The columns, rows and boxes are collectively called houses
type House = 
    | HColumn of Column
    | HRow of Row
    | HBox of Box
    override this.ToString() = 
        match this with
        | HColumn c -> c.ToString()
        | HRow r -> r.ToString()
        | HBox b -> b.ToString()

let houses (length : int<size>) (boxWidth : int<boxWidth>) (boxHeight : int<boxHeight>) = 
    let chs = List.map (fun c -> HColumn c) (columns length)

    let rhs = List.map (fun r -> HRow r) (rows length)

    let bhs = List.map (fun b -> HBox b) (boxes length boxWidth boxHeight)

    List.concat [ chs; rhs; bhs ]

// Each cell in the grid contains a Digit, usually numbers 1..9
type Digit = 
    | Digit of char
    override this.ToString() = 
        let (Digit s) = this
        (string) s

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the Digits in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type PuzzleShape = 
    { size : int<size>
      boxWidth : int<boxWidth>
      boxHeight : int<boxHeight>
      alphabet : Digit list }

// Whilst working to a solution each cell in the grid
// that doesn't have a Digit is filled with candidates
// Candidates are possible Digits
[<NoEquality; NoComparison>]
type CellContents = 
    | BigNumber of Digit
    | PencilMarks of Set<Digit>

type Value = 
    { cell : Cell
      digit : Digit }
    override this.ToString() = String.Format("{0}={1}", this.cell, this.digit)

// A candidate is a digit in a cell, which is still a pencilmark
type Candidate = 
    { cell : Cell
      digit : Digit }
    override this.ToString() = String.Format("({0}){1}", this.digit, this.cell)

// Working towards a solution we take one of the following actions:
// Set the cell to have a Digit
// or remove a candidate
type Action = 
    | Placement of Value
    | Eliminate of Candidate
    override this.ToString() =
        match this with
        | Placement a -> String.Format("{0}={1}", a.cell, a.digit)
        | Eliminate candidate -> String.Format("{0}<>{1}", candidate.cell, candidate.digit)

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
      setValueReduction : Digit option
      reductions : Set<Digit>
      pointers : Set<Digit> }

// From http://www.fssnip.net/ji
type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

let memoiseLookup (objects : 'a list) (lookup : 'a -> 'b) : ('a -> 'b) =
    let s = List.map (fun o -> (o, lookup o)) objects

    let s2 = s |> Map.ofList

    let memo = new System.Collections.Generic.Dictionary<'a, 'b>(s2)

    fun o -> memo.[o]
