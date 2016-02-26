module load

open core.sudoku

val load : cell list -> digit list -> char list -> (given -> current) -> solution
