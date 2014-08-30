module hints.fullHouse

open core.puzzlemap
open core.sudoku
open hints

type FullHouse = 
    { setCellValue : SetCellValue
      house : House }

val fullHouseFind : (Cell -> Set<Candidate>) -> PuzzleMaps -> FullHouse list

val fullHouseToDescription : FullHouse -> PuzzleMaps -> (Cell -> Set<Candidate>) -> HintDescription
