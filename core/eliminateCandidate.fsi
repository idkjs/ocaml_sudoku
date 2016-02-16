module core.eliminateCandidate

open sudoku

val eliminateCandidateApply : Candidate -> (Current -> Current)

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

val eliminateCandidateTry : Cell
     -> Digit -> Current -> Either<Candidate, EliminateCandidateError>
