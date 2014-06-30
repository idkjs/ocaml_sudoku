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

type Candidate = Candidate of char

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
    alphabet : Symbol list
    symbols : Cell->Symbol option
}

// Whilst working to a solution each cell in the grid
// has either:
type AnnotatedCandidate =
    | Possible
    | Excluded
    | Removed

[<NoEquality; NoComparison>]
type AnnotatedSymbol =
    | Given of Symbol
    | Set of Symbol
    | Candidates of (Candidate->AnnotatedCandidate)

[<NoEquality; NoComparison>]
type FormatLabel =
    | LPlain of AnnotatedSymbol
    | LHintHouse of AnnotatedSymbol
    | LHintCell of Symbol

type EntryLookup = Cell -> AnnotatedSymbol

type EntryLabel =
    | EGiven of Symbol
    | ESet of Symbol
    | EFLCandidatePossible of Candidate
    | EFLCandidateExcluded of Candidate

type FormatLabelF =
    | FLPlain of EntryLabel
    | FLHintHouse of EntryLabel
    | FLHintCell of Symbol
    | FLHintCandidatePointer of Candidate
    | FLHintCandidateReduction of Candidate

// Working towards a solution we take one of the following actions:

type SetCellValue =
    {
        cell:Cell
        value:Candidate
        //candidateReductions:Set<Cell>
    }

type Action =
    | SetValue of SetCellValue
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
