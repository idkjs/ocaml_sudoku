module core.setCell

open sudoku
open puzzlemap
open hints

val setCellDigitApply : CellHouseCells -> Value -> (Current -> Current)

val setCellDigitTry : Cell -> Digit -> CellCandidates -> Value option

val setCellHintDescription : PuzzleMap -> Value -> HintDescription2

val setCellStep : PuzzleMap -> Value -> Solution -> Solution
