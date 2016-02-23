module core.eliminateCandidate

open sudoku
open puzzlemap
open hints

let eliminateCandidateApply (candidate : Candidate) : Current -> Current = 

    let update (cell : Cell) (cellContents : CellContents) : CellContents = 
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Set.remove candidate.digit candidates)
            else cellContents

    Map.map update

let eliminateCandidateHintDescription (p: PuzzleMap) (candidate : Candidate) : HintDescription2 =
    let cr = 
        { CandidateReduction.cell = candidate.cell
          candidates = set [ candidate.digit ] }

    let hd = 
        { HintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set [ cr ]
          setCellValueAction = None
          pointers = set [] }

    mhas p.cells p.cellHouseCells p.houseCells hd

let eliminateCandidateStep (p : PuzzleMap) (candidate : Candidate) (solution : Solution) : Solution =
    { solution with current = eliminateCandidateApply candidate solution.current
                    steps = (Eliminate candidate) :: solution.steps }
