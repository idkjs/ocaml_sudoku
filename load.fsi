module load

open core.sudoku

val load : Digit list -> char list -> ((Cell -> Digit option) -> Cell -> CellContents) -> Solution
