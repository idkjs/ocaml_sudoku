module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

val nakedSingle : puzzleMap -> cellCandidates -> Set<hintDescription>

val nakedN : int -> puzzleMap -> cellCandidates -> Set<hintDescription>
