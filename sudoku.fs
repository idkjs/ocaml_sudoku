#light

module sudoku

open System
//open System.Text

// A sudoku is a grid of columns...
[<Measure>] type col

type Column = {
    col : int<col>
}

// ... by rows
[<Measure>] type row

type Row = {
    row : int<row>
}

// Each cell is identified by (col, row)
type Cell = {
    col : Column
    row : Row
}

// The grid is divided into boxes
[<Measure>] type boxcol

// A column of vertical boxes is a stack
type Stack = {
    stack : int<boxcol>
}

[<Measure>] type boxrow

// A row of horizontal boxes is a band
type Band = {
    band : int<boxrow>
}

// A box is the intersection of a stack and a band
type Box = {
    stack : Stack
    band : Band
}

// The columns, rows and boxes are called houses
type House =
    | Column of Column
    | Row of Row
    | Box of Box

// Each cell in the grid contains a symbol, usually numbers 1..9
type Symbol = Symbol of char

// A puzzle takes a list of symbols, which gives
// the size of the grid
type Alphabet = Symbol list

// Instead of defining the container, define how to get symbols per cell
type SymbolLookup = Cell -> Symbol option

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the symbols in the alphabet
// and also by the width and height of the boxes
type PuzzleSpec = {
    boxWidth : int
    boxHeight : int
    gridSize : int  // same as size of Alphabet
    alphabet : Alphabet
}

// Whilst working to a solution each cell in the grid
// has either:
type Entry =
    | Given of Symbol
    | Set of Symbol
    | Candidates of Set<Symbol>

type EntryLookup = Cell -> Entry

// Working towards a solution we take one of the following actions:
type Action =
    | Load
    | SetValue of Cell * Symbol
    | ClearCandidate of Cell * Symbol

// Track the updated grid and the action
type Step = {
    grid : EntryLookup
    action : Action
}

type Solution = {
    spec : PuzzleSpec
    puzzle : SymbolLookup
    mutable steps : Step list
}
