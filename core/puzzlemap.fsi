module core.puzzlemap

open sudoku

// for a column, return the cells in it
val columnCells : int<size> -> Column -> Cell list

// for a row, return the cells in it
val rowCells : int<size> -> Row -> Cell list

// for a column, which stack is it in?
val columnStack : int<boxWidth> -> Column -> Stack

// for a stack, return the columns in it
val stackColumns : int<boxWidth> -> Stack -> Column list

// for a row, which band is it in?
val rowBand : int<boxHeight> -> Row -> Band

// for a band, return the rows in it
val bandRows : int<boxHeight> -> Band -> Row list

// for a cell, which box is it in?
val cellBox : int<boxWidth> -> int<boxHeight> -> Cell -> Box

// for a box, return the cells in it
val boxCells : int<boxWidth> -> int<boxHeight> -> Box -> Cell list

// for a house, return the cells in it
val houseCells : int<size> -> int<boxWidth> -> int<boxHeight> -> House -> Set<Cell>

// for a cell, return the cells in the column, row and box it belongs to
val houseCellCells : int<size> -> int<boxWidth> -> int<boxHeight> -> Cell -> Set<Cell>
