module core.sudoku

open System

// A sudoku is a grid of columns...
[<Measure>]
type column

type Column = 
    { col : int<column> }
    override this.ToString() = String.Format("c{0}", this.col)

// ... by rows
[<Measure>]
type row

type Row = 
    { row : int<row> }
    override this.ToString() = String.Format("r{0}", this.row)

// Each cell is identified by (col, row)
type Cell = 
    { col : Column
      row : Row }
    override this.ToString() = String.Format("c{0}r{1}", (int) this.col.col, (int) this.row.row)

// The grid is divided into boxes
[<Measure>]
type boxcol

// A column of vertical boxes is a stack
type Stack = 
    { stack : int<boxcol> }
    override this.ToString() = String.Format("stk{0}", (int) this.stack)

[<Measure>]
type boxrow

// A row of horizontal boxes is a band
type Band = 
    { band : int<boxrow> }
    override this.ToString() = String.Format("bnd{0}", (int) this.band)

// A box is the intersection of a stack and a band
type Box = 
    { stack : Stack
      band : Band }
    override this.ToString() = String.Format("stk{0}bnd{1}", (int) this.stack.stack, (int) this.band.band)

// The columns, rows and boxes are called houses
type House = 
    | Column of Column
    | Row of Row
    | Box of Box
    override this.ToString() = 
        match this with
        | Column c -> c.ToString()
        | Row r -> r.ToString()
        | Box b -> b.ToString()

// Each cell in the grid contains a symbol, usually numbers 1..9
type Symbol = 
    | Symbol of char
    override this.ToString() = 
        let (Symbol s) = this
        (string) s

[<Measure>]
type width

[<Measure>]
type height

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the symbols in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type Puzzle = 
    { boxWidth : int<width>
      boxHeight : int<height>
      alphabet : Symbol list
      symbols : Cell -> Symbol option }

type Candidate = 
    | Candidate of char
    override this.ToString() = 
        let (Candidate c) = this
        (string) c

// Whilst working to a solution each cell in the grid
// that doesn't have a symbol is filled with candidates
type AnnotatedCandidate = 
    | Possible
    | Excluded
    | Removed

[<NoEquality; NoComparison>]
type AnnotatedSymbol<'a> = 
    | Given of Symbol
    | Set of Symbol
    | Candidates of (Candidate -> 'a)

type HintAnnotatedCandidate = 
    | HACId of AnnotatedCandidate
    | HACSet
    | Pointer
    | Reduction

[<NoEquality; NoComparison>]
type HintAnnotatedSymbol = 
    { symbol : AnnotatedSymbol<HintAnnotatedCandidate>
      hintHouse : bool }

// Working towards a solution we take one of the following actions:
type SetCellValue = 
    { cell : Cell
      candidate : Candidate }
    override this.ToString() = String.Format("SetCellValue: {0} = {1}", this.cell, this.candidate)

type ClearCandidate = 
    { cell : Cell
      value : Symbol }
    override this.ToString() = String.Format("ClearCandidate: {0} = {1}", this.cell, this.value)

type CandidateReduction = 
    { cell : Cell
      symbols : Set<Candidate> }
    override this.ToString() = 
        String.Format("Cell {0}, Candidates {1}", this.cell, String.Join(",", Set.toArray this.symbols))

type Action = 
    | SetCellValue of SetCellValue
    | ClearCandidate of ClearCandidate

[<NoEquality; NoComparison>]
type Solution = 
    { grid : Cell -> AnnotatedSymbol<AnnotatedCandidate>
      steps : Action list }
