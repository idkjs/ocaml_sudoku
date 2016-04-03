open Sudoku
open Puzzlemap

let apply (p : puzzleMap) (value : value) (current : current) : current = 

    let update (cell : cell) : cellContents =
        let cellContents = Current.get current cell in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = Smap.get Cell.comparer p.cellHouseCells value.cell in

            if value.cell = cell then BigNumber value.digit
            else if Cells.contains cell cells then 
                PencilMarks (Digits.remove value.digit candidates)
            else cellContents
        in

    Smap.ofLookup (Cells.toList p.cells) update
    |> Current.make

type setCellDigitError = 
    { cell : cell;
      candidate : digit;
      digit : digit }

let try' (cell : cell) (candidate : digit) (cellCandidates : cellCandidates) : value option = 
    let candidates = CellCandidates.get cellCandidates cell in

    if Digits.contains candidate candidates then
        Some (Value.make cell candidate)
    else None

let description (p : puzzleMap) (setCellValue : value) : Hint.description =
    { primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = [];
      setCellValueAction = Some setCellValue;
      pointers = [];
      focus = Digits.empty }

let step (p : puzzleMap) (setCellValue : value) (solution : solution) : solution =
    { solution with current = apply p setCellValue solution.current;
                    steps = (Placement setCellValue) :: solution.steps }
