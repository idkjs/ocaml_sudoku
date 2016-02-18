module core.puzzlemap

open sudoku

val makeColumn : int -> Column
val makeRow : int -> Row
val makeStack : int -> Stack
val makeBand : int -> Band

// The order they are normally written in

val orderedColumns : int<size> -> Column list

val orderedRows : int<size> -> Row list

val orderedCells : int<size> -> Cell list

val orderedStacks : int<size> -> int<boxWidth> -> Stack list

val orderedBands : int<size> -> int<boxHeight> -> Band list

// for a stack, return the columns in it
val orderedStackColumns : int<boxWidth> -> Stack -> Column list

// for a band, return the rows in it
val orderedBandRows : int<boxHeight> -> Band -> Row list

val columns : int<size> -> Set<Column>

val rows : int<size> -> Set<Row>

val cells : int<size> -> Set<Cell>

val stacks : int<size> -> int<boxWidth> -> Set<Stack>

val bands : int<size> -> int<boxHeight> -> Set<Band>

val boxes : int<size> -> int<boxWidth> -> int<boxHeight> -> Set<Box>

val houses : int<size> -> int<boxWidth> -> int<boxHeight> -> Set<House>

// for a column, return the cells in it
val columnCells : int<size> -> Column -> Set<Cell>

// for a row, return the cells in it
val rowCells : int<size> -> Row -> Set<Cell>

// for a column, which stack is it in?
val columnStack : int<boxWidth> -> Column -> Stack

// for a stack, return the columns in it
val stackColumns : int<boxWidth> -> Stack -> Set<Column>

// for a row, which band is it in?
val rowBand : int<boxHeight> -> Row -> Band

// for a band, return the rows in it
val bandRows : int<boxHeight> -> Band -> Set<Row>

// for a cell, which box is it in?
val cellBox : int<boxWidth> -> int<boxHeight> -> Cell -> Box

// for a box, return the cells in it
val boxCells : int<boxWidth> -> int<boxHeight> -> Box -> Set<Cell>

// for a house, return the cells in it
val houseCells : int<size> -> int<boxWidth> -> int<boxHeight> -> House -> Set<Cell>

// for a cell, return the cells in the column, row and box it belongs to
type MapCellHouseCells = Map<Cell, Set<Cell>>

val houseCellCells : int<size> -> int<boxWidth> -> int<boxHeight> -> MapCellHouseCells
