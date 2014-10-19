module core.eliminateCandidate

open sudoku

let eliminateCandidateApply (candidate : Candidate) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell

        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Set.remove candidate.digit candidates)
            else cellContents

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

let eliminateCandidateTry (cell : Cell) (digit : Digit) (cellCellContents : Cell -> CellContents) : Either<Candidate, EliminateCandidateError> = 
    match cellCellContents cell with
    | BigNumber digit -> Right(AlreadySet digit)
    | PencilMarks digits -> 
        if Set.contains digit digits then 
            Left { Candidate.cell = cell
                   digit = digit }
        else Right NotACandidate
