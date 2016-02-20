module core.puzzlemap

open sudoku

val makeColumn : int -> Column
val makeRow : int -> Row
val makeStack : int -> Stack
val makeBand : int -> Band

type ColumnCells = Lookup<Column, Set<Cell>>

type RowCells = Lookup<Row, Set<Cell>>

type ColumnStack = Lookup<Column, Stack>

type StackColumns = Lookup<Stack, Set<Column>>

type RowBand = Lookup<Row, Band>

type BandRows = Lookup<Band, Set<Row>>

type CellBox = Lookup<Cell, Box>

type BoxCells = Lookup<Box, Set<Cell>>

// for a house, return the cells in it
type HouseCells = Lookup<House, Set<Cell>>

// for a cell, return the cells in the column, row and box it belongs to
type CellHouseCells =Lookup<Cell, Set<Cell>>

type PuzzleMap =
    // The order they are normally written in
    abstract member orderedColumns : Column list
    abstract member orderedRows : Row list
    abstract member orderedCells : Cell list
    abstract member orderedStacks : Stack list
    abstract member orderedBands : Band list

    // for a stack, return the columns in it
    abstract member orderedStackColumns : Stack -> Column list

    // for a band, return the rows in it
    abstract member orderedBandRows : Band -> Row list

    abstract member columns : Set<Column>

    abstract member rows : Set<Row>

    abstract member cells : Set<Cell>

    abstract member stacks : Set<Stack>

    abstract member bands : Set<Band>

    abstract member boxes : Set<Box>

    abstract member houses : Set<House>

    // for a column, return the cells in it
    abstract member columnCells : ColumnCells

    // for a row, return the cells in it
    abstract member rowCells : RowCells

    // for a column, which stack is it in?
    abstract member columnStack : ColumnStack

    // for a stack, return the columns in it
    abstract member stackColumns : StackColumns

    // for a row, which band is it in?
    abstract member rowBand : RowBand

    // for a band, return the rows in it
    abstract member bandRows : BandRows

    // for a cell, which box is it in?
    abstract member cellBox : CellBox

    // for a box, return the cells in it
    abstract member boxCells : BoxCells

    // for a house, return the cells in it
    abstract member houseCells : HouseCells

    abstract member cellHouseCells : CellHouseCells

type TPuzzleMap =
    interface PuzzleMap

    new : PuzzleShape -> TPuzzleMap
