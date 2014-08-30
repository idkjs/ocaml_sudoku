module hints.hiddenSingle

open core.puzzlemap
open core.sudoku
open hints

type HiddenSingle = 
    { setCellValue : SetCellValue
      house : House }

val hiddenSingleFind : Candidate list -> (Cell -> Set<Candidate>) -> PuzzleMaps -> HiddenSingle list

val hiddenSingleToDescription : HiddenSingle -> PuzzleMaps -> (Cell -> Set<Candidate>) -> HintDescription
