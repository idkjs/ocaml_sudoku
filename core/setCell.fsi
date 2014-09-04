module core.setCell

open puzzlemap
open sudoku

val setCellValueModelEffect : SetCellValue -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>

val setCellCandidateReductions : SetCellValue
     -> (Cell -> Set<Cell>) -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>

val setCellApply : SetCellValue
     -> (Cell -> Set<Cell>)
     -> (Cell -> Set<Candidate>)
     -> ((Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> AnnotatedSymbol<AnnotatedCandidate>)

val setCellTry : Candidate -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> SetCellValue option

