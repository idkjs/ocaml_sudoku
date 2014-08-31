module hints.nakedSingle

open core.sudoku
open hints

type NakedSingle = 
    { setCellValue : SetCellValue }

val nakedSingleFind : (Cell -> Set<Candidate>) -> Cell list -> NakedSingle list

val nakedSingleToDescription : NakedSingle -> HintDescription

