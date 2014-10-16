module core.eliminateCandidate

open sudoku

let eliminateCandidateApply (eliminateCandidate : EliminateCandidateAction) : (Cell -> CellContents) -> Cell -> CellContents = 

    fun (cellCellContents : Cell -> CellContents) (cell : Cell) -> 
        let cellContents = cellCellContents cell

        match cellContents with
        | ADigit _ -> cellContents
        | ACandidates candidates -> 
            if eliminateCandidate.cell = cell then ACandidates(Set.remove eliminateCandidate.candidate candidates)
            else cellContents

type EliminateCandidateError = 
    | AlreadySet of Digit
    | NotACandidate

let eliminateCandidateTry (cell : Cell) (candidate : Candidate) (cellCellContents : Cell -> CellContents) : Either<EliminateCandidateAction, EliminateCandidateError> = 
    match cellCellContents cell with
    | ADigit digit -> Right(AlreadySet digit)
    | ACandidates candidates -> 
        if Set.contains candidate candidates then 
            Left { EliminateCandidateAction.cell = cell
                   candidate = candidate }
        else Right NotACandidate
