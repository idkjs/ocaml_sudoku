module core.eliminateCandidate

open sudoku

val eliminateCandidateApply : EliminateCandidateAction -> (Cell -> CellContents) -> Cell -> CellContents

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

val eliminateCandidateTry : Cell
     -> Candidate -> (Cell -> CellContents) -> Either<EliminateCandidateAction, EliminateCandidateError>
