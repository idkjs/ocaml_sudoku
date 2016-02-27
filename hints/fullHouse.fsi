module hints.fullHouse

open core.sudoku
open core.puzzlemap
open core.hints

val fullHouses : puzzleMap -> cellCandidates -> Set<hintDescription>
