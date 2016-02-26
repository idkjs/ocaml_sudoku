module core.eliminateCandidate

open sudoku
open puzzlemap
open hints

let eliminateCandidateApply (candidate : candidate) : current -> current = 

    let update (cell : cell) (cellContents : cellContents) : cellContents = 
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Set.remove candidate.digit candidates)
            else cellContents

    Map.map update

let eliminateCandidateHintDescription (p: puzzleMap) (candidate : candidate) : hintDescription =
    let cr = 
        { candidateReduction.cell = candidate.cell
          candidates = set [ candidate.digit ] }

    let hd = 
        { hintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set [ cr ]
          setCellValueAction = None
          pointers = set []
          focus = set [] }

    hd

let eliminateCandidateStep (p : puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = eliminateCandidateApply candidate solution.current
                    steps = (Eliminate candidate) :: solution.steps }
