module hints.wing

open core.sudoku
open core.puzzlemap
open core.hints

val xWings : puzzleMap -> cellCandidates -> Set<hintDescription>

val yWings : puzzleMap -> cellCandidates -> Set<hintDescription>
