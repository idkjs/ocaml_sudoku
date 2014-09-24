module core.setCell

open puzzlemap
open sudoku

val setCellValueModelEffect : SetCellValue -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>

val setCellCandidateReductions : SetCellValue
     -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>

val setCellApply : SetCellValue
     -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> ((Cell -> CellContents) -> Cell -> CellContents)
val setCellTry : Candidate -> (Cell -> CellContents) -> Cell -> SetCellValue option

