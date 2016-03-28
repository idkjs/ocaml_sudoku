open Smap
open Sudoku
open Puzzlemap
open Hints

let eliminateCandidateApply (p : puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = Current.get current cell in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Digits.remove candidate.digit candidates)
            else cellContents
        in

    SMap.ofLookup<cell, cellContents> (Cells.toList p.cells) update
    |> Current

let eliminateCandidateHintDescription (p: puzzleMap) (candidate : candidate) : hintDescription =
    let cr = CandidateReduction.make (candidate.cell) (Digits.singleton candidate.digit) in

    { hintDescription.primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = [cr];
      setCellValueAction = None;
      pointers = [];
      focus = Digits.empty }

let eliminateCandidateStep (p : puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = eliminateCandidateApply p candidate solution.current;
                    steps = (Eliminate candidate) :: solution.steps }
