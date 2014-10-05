module core.puzzlemap

open sudoku

// for a column, return the cells in it
val columnCells : int<size> -> Column -> Cell list

// for a row, return the cells in it
val rowCells : int<size> -> Row -> Cell list

// for a column, which stack is it in?
val columnStack : int<width> -> Column -> Stack

// for a stack, return the columns in it
val stackColumns : int<width> -> Stack -> Column list

// for a row, which band is it in?
val rowBand : int<height> -> Row -> Band

// for a band, return the rows in it
val bandRows : int<height> -> Band -> Row list

// for a cell, which box is it in?
val cellBox : int<width> -> int<height> -> Cell -> Box

// for a box, return the cells in it
val boxCells : int<width> -> int<height> -> Box -> Cell list

// for a house, return the cells in it
val houseCells : int<size> -> int<width> -> int<height> -> House -> Set<Cell>

// for a cell, return the cells in the column, row and box it belongs to
val houseCellCells : int<size> -> int<width> -> int<height> -> Cell -> Set<Cell>
