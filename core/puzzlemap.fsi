module core.puzzlemap

open sudoku

val konst : 'a -> 'b -> 'a

val columnCells : int<size> -> Column -> Cell list

val rowCells : int<size> -> Row -> Cell list

val columnStack : int<width> -> Column -> Stack

val stackColumns : int<width> -> Stack -> Column list

val rowBand : int<height> -> Row -> Band

val bandRows : int<height> -> Band -> Row list

val cellBox : int<width> -> int<height> -> Cell -> Box

val boxCells : int<width> -> int<height> -> Box -> Cell list

val houseCells : int<size> -> int<width> -> int<height> -> House -> Set<Cell>

val houseCellCells : int<size> -> int<width> -> int<height> -> Cell -> Set<Cell>
