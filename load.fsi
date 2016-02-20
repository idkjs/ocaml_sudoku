module load

open core.sudoku

val load : Cell list -> Digit list -> char list -> (Given -> Current) -> Solution
