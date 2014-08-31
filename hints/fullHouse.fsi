module hints.fullHouse

open core.sudoku
open hints

type FullHouse = 
    { setCellValue : SetCellValue
      house : House }

val fullHouseFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> FullHouse list

val fullHouseToDescription : FullHouse -> HintDescription
