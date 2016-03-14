module core.puzzlemap

open smap
open sudoku

val makeColumn : int -> column

val makeRow : int -> row

val makeCell : column -> row -> cell

val makeStack : int -> stack

val makeBand : int -> band

val makeBox : stack -> band -> box

val makeValue : cell -> digit -> value

val makeCandidate : cell -> digit -> candidate

val makeCandidateReduction : cell -> digits -> candidateReduction

[<NoComparisonAttribute;NoEqualityAttribute>]
type puzzleMap =
    {
        columns : column array
        rows : row array
        cells : cell array
        stacks : stack array
        bands : band array
        boxes : box array
        houses : house array
        (* for a column, return the cells in it *)
        columnCells : SMap<column, cell array>
        (* for a row, return the cells in it *)
        rowCells : SMap<row, cell array>
        (* for a column, which stack is it in? *)
        columnStack : SMap<column, stack>
        (* for a stack, return the columns in it *)
        stackColumns : SMap<stack, column array>
        (* for a row, which band is it in? *)
        rowBand : SMap<row, band>
        (* for a band, return the rows in it *)
        bandRows : SMap<band, row array>
        (* for a cell, which box is it in? *)
        cellBox : SMap<cell, box>
        (* for a box, return the cells in it *)
        boxCells : SMap<box, cell array>
        (* for a house, return the cells in it *)
        houseCells : SMap<house, cells>
        cellHouseCells : SMap<cell, cells>
        housesCells : houses -> cells
        houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

        //abstract member houseCellCandidates : SMap<house, cellCandidates>
    }

val tPuzzleMap : puzzleShape -> puzzleMap
