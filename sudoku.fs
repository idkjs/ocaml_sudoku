module sudoku

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

[<Measure>] type width
[<Measure>] type height

// A sudoku is defined by the overall grid size (it is always square)
// which is the same as the symbols in the alphabet
// and also by the width and height of the boxes
[<NoEquality; NoComparison>]
type Puzzle = {
    boxWidth : int<width>
    boxHeight : int<height>
    alphabet : Alphabet
    symbols : SymbolLookup
}

// Whilst working to a solution each cell in the grid
// has either:
type Entry =
    | Given of Symbol
    | Set of Symbol
    | Candidates of Set<Symbol>

type EntryLookup = Cell -> Entry

type CandidateLookup = Cell -> Set<Symbol>

type FormatLabel =
    | LPlain of Entry
    | LHintHouse of Entry
    | LHintCell of Symbol

type FormatLabelF =
    | FLGiven of Symbol
    | FLSet of Symbol
    | FLCandidatePossible of Symbol
    | FLCandidateExcluded of Symbol
    | FLHintHouseGiven of Symbol
    | FLHintHouseSet of Symbol
    | FLHintCell of Symbol
    | FLHintCandidatePointer of Symbol
    | FLHintCandidateReduction of Symbol

// Working towards a solution we take one of the following actions:
type Action =
    | SetValue of Cell * Symbol
    | ClearCandidate of Cell * Symbol

[<NoEquality; NoComparison>]
type Solution = {
    grid : EntryLookup
    steps : Action list
}

[<NoEquality; NoComparison>]
type PuzzleMaps = {
    columns : Column list
    rows : Row list
    cells : Cell list

    cellColumn : Cell->Column
    columnCells : Column->Cell list
    cellRow : Cell->Row
    rowCells : Row->Cell list
    cellBox : Cell -> Box
    boxCells : Box -> Cell list

    stacks : Stack list
    bands : Band list
    boxes : Box list

    columnStack : Column -> Stack
    stackColumns : Stack-> Column list
    rowBand : Row->Band
    bandRows : Band->Row list
}
