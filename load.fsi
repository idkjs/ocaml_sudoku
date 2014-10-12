module load

open core.sudoku

val load : Symbol list -> char list -> ((Cell -> Symbol option) -> Cell -> CellContents) -> Solution
