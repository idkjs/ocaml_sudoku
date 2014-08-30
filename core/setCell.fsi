module core.setCell

open puzzlemap
open sudoku

val setCellApply : SetCellValue
     -> PuzzleMaps
     -> (Cell -> Set<Candidate>)
     -> ((Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate>)

val setCellTry : PuzzleMaps -> Candidate -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> SetCellValue option
