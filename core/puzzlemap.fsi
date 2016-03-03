module core.puzzlemap

open sudoku

val makeColumn : int -> column

val makeRow : int -> row

val makeStack : int -> stack

val makeBand : int -> band

val makeSetCellDigit : cell -> digit -> value

val makeCandidate : cell -> digit -> candidate

type puzzleMap =

    abstract member columns : column array

    abstract member rows : row array

    abstract member cells : cell array

    abstract member stacks : stack array

    abstract member bands : band array

    abstract member boxes : box array

    abstract member houses : house array

    (* for a column, return the cells in it *)
    abstract member columnCells : lookup<column, cell array>

    (* for a row, return the cells in it *)
    abstract member rowCells : lookup<row, cell array>

    (* for a column, which stack is it in? *)
    abstract member columnStack : lookup<column, stack>

    (* for a stack, return the columns in it *)
    abstract member stackColumns : lookup<stack, column array>

    (* for a row, which band is it in? *)
    abstract member rowBand : lookup<row, band>

    (* for a band, return the rows in it *)
    abstract member bandRows : lookup<band, row array>

    (* for a cell, which box is it in? *)
    abstract member cellBox : lookup<cell, box>

    (* for a box, return the cells in it *)
    abstract member boxCells : lookup<box, cell array>

    (* for a house, return the cells in it *)
    abstract member houseCells : lookup<house, cells>

    abstract member cellHouseCells : lookup<cell, cells>

    abstract member housesCells : houses -> cells

    abstract member houseCellCandidateReductions : house -> cellCandidates -> candidateReductions

    //abstract member houseCellCandidates : lookup<house, cellCandidates>

type tPuzzleMap =
    interface puzzleMap

    new : puzzleShape -> tPuzzleMap
