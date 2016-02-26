module core.force

open sudoku
open puzzlemap

val solve : solution -> Set<cell> -> cellHouseCells -> Set<solution>
