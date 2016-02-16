module core.eliminateCandidate

open sudoku

let eliminateCandidateApply (candidate : Candidate) : Current -> Current = 

    let update (cell : Cell) (cellContents : CellContents) : CellContents = 
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Set.remove candidate.digit candidates)
            else cellContents

    Map.map update

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

let eliminateCandidateTry (cell : Cell) (digit : Digit) (current : Current) : Either<Candidate, EliminateCandidateError> = 
    match current.Item cell with
    | BigNumber digit -> Right(AlreadySet digit)
    | PencilMarks digits -> 
        if Set.contains digit digits then 
            Left { Candidate.cell = cell
                   digit = digit }
        else Right NotACandidate
