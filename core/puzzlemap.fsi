module core.puzzlemap

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
        columnCells : lookup<column, cell array>
        (* for a row, return the cells in it *)
        rowCells : lookup<row, cell array>
        (* for a column, which stack is it in? *)
        columnStack : lookup<column, stack>
        (* for a stack, return the columns in it *)
        stackColumns : lookup<stack, column array>
        (* for a row, which band is it in? *)
        rowBand : lookup<row, band>
        (* for a band, return the rows in it *)
        bandRows : lookup<band, row array>
        (* for a cell, which box is it in? *)
        cellBox : lookup<cell, box>
        (* for a box, return the cells in it *)
        boxCells : lookup<box, cell array>
        (* for a house, return the cells in it *)
        houseCells : lookup<house, cells>
        cellHouseCells : lookup<cell, cells>
        housesCells : houses -> cells
        houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

        //abstract member houseCellCandidates : lookup<house, cellCandidates>
    }

val tPuzzleMap : puzzleShape -> puzzleMap
