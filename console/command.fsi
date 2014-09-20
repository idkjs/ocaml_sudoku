module console.command

open core.sudoku

val setCellCommand : string
     -> Candidate list -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell list -> SetCellValue option

val candidateClearCommand : string
     -> Candidate list -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell list -> ClearCandidate option
