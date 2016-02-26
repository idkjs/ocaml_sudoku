module core.sudoku

open System

// A sudoku is a square grid of size...
type size = int

// containing columns...
type Column = 
    | CColumn of int
    override this.ToString() =
        match this with CColumn c -> String.Format("c{0}", c)

// ... by rows
type Row = 
    | RRow of int
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
// A column of vertical boxes is a stack
type Stack = 
    | SStack of int
    override this.ToString() =
        match this with SStack s -> String.Format("stk{0}", s)

type boxWidth = int

// A row of horizontal boxes is a band
type Band = 
    | BBand of int
    override this.ToString() =
        match this with BBand b -> String.Format("bnd{0}", b)

type boxHeight = int

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
    { size : size
      boxWidth : boxWidth
      boxHeight : boxHeight
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

// From http://www.fssnip.net/ji
type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

type Lookup<'a, 'b> = 
    abstract member Get: 'a -> 'b

type MapLookup<'a, 'b when 'a : comparison>(data : Map<'a, 'b>) =
    interface Lookup<'a, 'b> with
        member this.Get (a : 'a) =
            data.Item a

// for a cell, return a set of candidates
type CellCandidates = Lookup<Cell, Set<Digit>>

let currentCellCandidates (current : Current) : CellCandidates =
    let getCandidateEntries (_ : Cell) (annotatedDigit : CellContents) : Set<Digit> =
        match annotatedDigit with
        | BigNumber _ -> Set.empty
        | PencilMarks s -> s

    let candidateLookup =
        current
        |> Map.map getCandidateEntries

    MapLookup<Cell, Set<Digit>> candidateLookup :> CellCandidates
