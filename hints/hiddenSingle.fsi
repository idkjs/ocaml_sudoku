module hints.hiddenSingle

open core.sudoku
open hints

type HiddenSingle = 
    { setCellValue : SetCellValue
      house : House }

val hiddenSingleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HiddenSingle list

val hiddenSingleToDescription : HiddenSingle -> HintDescription
