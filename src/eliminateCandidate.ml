open Sudoku
open Puzzlemap
open Hint

let apply (p : puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Digits.remove candidate.digit candidates)
            else cellContents
        in

    Smap.ofLookup update (Cells.to_list p.cells)
    |> Current.make

let description (p: puzzleMap) (candidate : candidate) : Hint.description =
    let cr = CandidateReduction.make (candidate.cell) (Digits.singleton candidate.digit) in

    { primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = [cr];
      setCellValueAction = None;
      pointers = [];
      focus = Digits.empty }

let step (p : puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = apply p candidate solution.current;
                    steps = (Eliminate candidate) :: solution.steps }
