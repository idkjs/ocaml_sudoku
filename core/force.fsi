module core.force

open sudoku

val solve : Solution -> Cell list -> (Cell -> Set<Cell>) -> Solution list
