module core.eliminateCandidate

open sudoku

val eliminateCandidateApply : Candidate -> (Cell -> CellContents) -> Cell -> CellContents

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

val eliminateCandidateTry : Cell
     -> Digit -> (Cell -> CellContents) -> Either<Candidate, EliminateCandidateError>
