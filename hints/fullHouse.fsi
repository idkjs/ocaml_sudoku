module hints.fullHouse

open core.sudoku
open core.puzzlemap
open core.hints

val fullHouses : puzzleMap -> cellCandidates -> hintDescription array
