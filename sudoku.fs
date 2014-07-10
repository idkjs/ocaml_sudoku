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
// that doesn't have a symbol is filled with candidates
type AnnotatedCandidate =
    | Possible
    | Excluded
    | Removed

[<NoEquality; NoComparison>]
type AnnotatedSymbol =
    | Given of Symbol
    | Set of Symbol
    | Candidates of (Candidate->AnnotatedCandidate)

type HintAnnotatedCandidate =
    | HACId of AnnotatedCandidate
    | HACSet
    | Pointer
    | Reduction
    | HACHouse

[<NoEquality; NoComparison>]
type HintAnnotatedSymbol =
    | HASId of AnnotatedSymbol
    | HASHouse of AnnotatedSymbol
    | HASCell of Candidate
    | FLHintCandidates of (Candidate->HintAnnotatedCandidate)

// Working towards a solution we take one of the following actions:
type SetCellValue =
    {
        cell:Cell
        value:Candidate
    }

type ClearCandidate =
    {
        cell:Cell
        value:Symbol
    }

type FullHouse =
    {
        cell:Cell
        candidate:Candidate
        house:House
    }

type HiddenSingle =
    {
        cell:Cell
        symbol:Candidate
        house:House
    }

type NakedSingle =
    {
        cell:Cell
        symbol:Candidate
    }

type CandidateReduction = {
    cell:Cell
    symbols:Set<Candidate>
}

type NakedPair = {
    cell1:Cell
    cell2:Cell
    symbols:Set<Candidate>
    candidateReduction:CandidateReduction list
    house:House
}

type Hint =
    | FH of FullHouse
    | HS of HiddenSingle
    | NS of NakedSingle
    | NP of NakedPair

type Action =
    | SetValue of SetCellValue
    | ClearCandidate of ClearCandidate
    | ApplyHint of Hint

[<NoEquality; NoComparison>]
type Solution = {
    grid : Cell->AnnotatedSymbol
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
    cellBox : Cell->Box
    boxCells : Box->Cell list

    stacks : Stack list
    bands : Band list
    boxes : Box list

    columnStack : Column -> Stack
    stackColumns : Stack-> Column list
    rowBand : Row->Band
    bandRows : Band->Row list
}
