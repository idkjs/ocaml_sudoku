module hints.fullHouse

open core.sudoku
open core.puzzlemap

open hints

val fullHouses : PuzzleMap -> CellCandidates -> Set<HintDescription2>
