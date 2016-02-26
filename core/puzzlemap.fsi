module core.puzzlemap

open sudoku

val makeColumn : int -> column
val makeRow : int -> row
val makeStack : int -> stack
val makeBand : int -> band

val makeSetCellDigit : cell -> digit -> value

val makeCandidate : cell -> digit -> candidate

type columnCells = lookup<column, Set<cell>>

type rowCells = lookup<row, Set<cell>>

type columnStack = lookup<column, stack>

type stackColumns = lookup<stack, Set<column>>

type rowBand = lookup<row, band>

type bandRows = lookup<band, Set<row>>

type cellBox = lookup<cell, box>

type boxCells = lookup<box, Set<cell>>

// for a house, return the cells in it
type houseCells = lookup<house, Set<cell>>

// for a cell, return the cells in the column, row and box it belongs to
type cellHouseCells =lookup<cell, Set<cell>>

type puzzleMap =
    // The order they are normally written in
    abstract member orderedColumns : column list
    abstract member orderedRows : row list
    abstract member orderedCells : cell list
    abstract member orderedStacks : stack list
    abstract member orderedBands : band list

    // for a stack, return the columns in it
    abstract member orderedStackColumns : stack -> column list

    // for a band, return the rows in it
    abstract member orderedBandRows : band -> row list

    abstract member columns : Set<column>

    abstract member rows : Set<row>

    abstract member cells : Set<cell>

    abstract member stacks : Set<stack>

    abstract member bands : Set<band>

    abstract member boxes : Set<box>

    abstract member houses : Set<house>

    // for a column, return the cells in it
    abstract member columnCells : columnCells

    // for a row, return the cells in it
    abstract member rowCells : rowCells

    // for a column, which stack is it in?
    abstract member columnStack : columnStack

    // for a stack, return the columns in it
    abstract member stackColumns : stackColumns

    // for a row, which band is it in?
    abstract member rowBand : rowBand

    // for a band, return the rows in it
    abstract member bandRows : bandRows

    // for a cell, which box is it in?
    abstract member cellBox : cellBox

    // for a box, return the cells in it
    abstract member boxCells : boxCells

    // for a house, return the cells in it
    abstract member houseCells : houseCells

    abstract member cellHouseCells : cellHouseCells

type tPuzzleMap =
    interface puzzleMap

    new : puzzleShape -> tPuzzleMap
