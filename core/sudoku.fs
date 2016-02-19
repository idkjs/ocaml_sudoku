module core.sudoku

open System

// A sudoku is a square grid of size...
[<Measure>]
type size

// containing columns...
[<Measure>]
type column

type Column = 
    | CColumn of int<column>
    override this.ToString() =
        match this with CColumn c -> String.Format("c{0}", c)

// ... by rows
[<Measure>]
type row

type Row = 
    | RRow of int<row>
    override this.ToString() =
        match this with RRow r -> String.Format("r{0}", r)

// Each cell is identified by (col, row)
type Cell = 
    { col : Column
      row : Row }
    override this.ToString() =
        String.Format("{0}{1}", this.row, this.col)

// The grid is divided into boxes,
// these do not have to be square, but they are
// all the same size and cover the grid
[<Measure>]
type stack

// A column of vertical boxes is a stack
type Stack = 
    | SStack of int<stack>
    override this.ToString() =
        match this with SStack s -> String.Format("stk{0}", s)

[<Measure>]
type boxWidth

[<Measure>]
type band

// A row of horizontal boxes is a band
type Band = 
    | BBand of int<band>
    override this.ToString() =
        match this with BBand b -> String.Format("bnd{0}", b)

[<Measure>]
type boxHeight

// A box is the intersection of a stack and a band
type Box = 
    { stack : Stack
      band : Band }
    override this.ToString() =
        String.Format("{0}{1}", this.stack, this.band)

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

// Each cell in the grid contains a Digit, usually numbers 1..9
type Digit = 
    | Digit of char
    override this.ToString() =
        match this with Digit s -> (string) s

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the Digits in the alphabet
// and also by the width and height of the boxes
type PuzzleShape = 
    { size : int<size>
      boxWidth : int<boxWidth>
      boxHeight : int<boxHeight>
      alphabet : Digit list }

// Whilst working to a solution each cell in the grid
// that doesn't have a Digit is filled with candidates
// Candidates are possible Digits
type CellContents = 
    | BigNumber of Digit
    | PencilMarks of Set<Digit>

// Working towards a solution we take one of the following actions:
// Set the cell to have a Digit
type Value = 
    { cell : Cell
      digit : Digit }
    override this.ToString() =
        String.Format("{0}={1}", this.cell, this.digit)

// A candidate is a digit in a cell, which is still a pencilmark
type Candidate = 
    { cell : Cell
      digit : Digit }
    override this.ToString() =
        String.Format("({0}){1}", this.digit, this.cell)

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

type Given = Map<Cell, Digit option>

type Current = Map<Cell, CellContents>

type Solution = 
    { given : Given
      current : Current
      steps : Action list }

// To draw a cell we may want to display extra information...
type CellAnnotation = 
    { setValue : Digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : Digit option
      reductions : Set<Digit>
      pointers : Set<Digit> }

type Annotations = Map<Cell, CellAnnotation>

// From http://www.fssnip.net/ji
type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

// for a cell, return a set of candidates
type CellCandidates =
    abstract member Get: Cell -> Set<Digit>

type MapCellCandidates(data : Map<Cell, Set<Digit>>) =
    interface CellCandidates with
        member this.Get (cell : Cell) =
            data.Item cell

// for a cell, return the cells in the column, row and box it belongs to
type CellHouseCells =
    abstract member Get: Cell -> Set<Cell>

type MapCellHouseCells(data : Map<Cell, Set<Cell>>) =
    interface CellHouseCells with
        member this.Get (cell : Cell) =
            data.Item cell

// for a house, return the cells in it
type HouseCells =
    abstract member Get: House -> Set<Cell>

type MapHouseCells(data : Map<House, Set<Cell>>) =
    interface HouseCells with
        member this.Get (house : House) =
            data.Item house

// for a cell, return the box it is in
type CellBox =
    abstract member Get: Cell -> Box

type MapCellBox(data : Map<Cell, Box>) =
    interface CellBox with
        member this.Get (cell : Cell) =
            data.Item cell
