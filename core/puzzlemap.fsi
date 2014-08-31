module core.puzzlemap

open sudoku

val konst : 'a -> 'b -> 'a

val symbolToCandidate : Symbol -> Candidate

val columns : int -> Column list

val rows : int -> Row list

val cells : int -> Cell list

val columnCells : int -> Column -> Cell list

// For a cell, return all the cell symbols for its containing row
val columnCellCells : int -> Cell -> Cell list

val rowCells : int -> Row -> Cell list

// For a cell, return all the cell symbols for its containing column
val rowCellCells : int -> Cell -> Cell list

val stacks : int -> int<width> -> Stack list

val columnStack : int<width> -> Column -> Stack

val stackColumns : int<width> -> Stack -> Column list

val bands : int -> int<height> -> Band list

val rowBand : int<height> -> Row -> Band

val bandRows : int<height> -> Band -> Row list

val boxes : int -> int<width> -> int<height> -> Box list

val cellBox : int<width> -> int<height> -> Cell -> Box

val boxCells : int<width> -> int<height> -> Box -> Cell list

// For a cell, return all the cell symbols for its containing box
val boxCellCells : int<width> -> int<height> -> Cell -> Cell list

val houses : int -> int<width> -> int<height> -> House list

val houseCells : int -> int<width> -> int<height> -> House -> Set<Cell>

val houseCellCells : int -> int<width> -> int<height> -> Cell -> Set<Cell>

val houseCellsForCell : int -> int<width> -> int<height> -> Cell -> (Cell -> 'a option) -> Set<'a>
