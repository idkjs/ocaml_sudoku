module eliminateCandidate

open smap
open sudoku
open puzzlemap
open hints

let eliminateCandidateApply (p : puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = SMap.get current cell
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Digits.remove candidate.digit candidates)
            else cellContents

    SMap.ofLookup<cell, cellContents> p.cells update

let eliminateCandidateHintDescription (p: puzzleMap) (candidate : candidate) : hintDescription =
    let cr = makeCandidateReduction (candidate.cell) (Digits.singleton candidate.digit)

    { hintDescription.primaryHouses = Houses.empty
      secondaryHouses = Houses.empty
      candidateReductions = CandidateReductions.singleton cr
      setCellValueAction = None
      pointers = CandidateReductions.empty
      focus = Digits.empty }

let eliminateCandidateStep (p : puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = eliminateCandidateApply p candidate solution.current
                    steps = (Eliminate candidate) :: solution.steps }
