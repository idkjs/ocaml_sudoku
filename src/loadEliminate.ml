open Sudoku
open Puzzlemap
open Hint

let find  (p : puzzleMap) (current : current) : candidateReduction list = 

    let reductions (cell : cell) : digits option =
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                p.cellHouseCells
                |> Smap.get Cell.comparer cell
                |> Cells.choose
                    (fun cell ->
                        let houseCellContents = Current.get cell current in
                        match houseCellContents with
                        | BigNumber digit -> Some digit
                        | PencilMarks _ -> None)
                |> Digits.make in

            if Digits.count digits > 0 then Some digits
            else None
        in

    p.cells
    |> Cells.choose
        (fun cell ->
            match reductions cell with
            | Some digits -> Some (CandidateReduction.make cell digits)
            | None -> None)

let apply (p : puzzleMap) (candidateReductions : candidateReduction list) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
        |> List.map (fun cr -> (cr.cell, cr.candidates))
        in

    let update (cell : cell) : cellContents =
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates ->
            let digitsOpt = Smap.tryGet Cell.comparer cell candidateReductionsLookup in
            match digitsOpt with
            | Some digits ->
                Digits.difference candidates digits
                |> CellContents.make_pencil_marks
            | None -> cellContents
        in

    Cells.ofLookup update p.cells
    |> Current.make

let description (p : puzzleMap) (candidateReductions : candidateReduction list) : Hint.description =
    { primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = candidateReductions;
      setCellValueAction = None;
      pointers = [];
      focus = Digits.empty }

let step (p : puzzleMap) (solution : solution) (candidateReductions : candidateReduction list) : solution =
    { solution with current = apply p candidateReductions solution.current;
                    steps = LoadEliminate :: solution.steps }

let findAndApply (p : puzzleMap) (solution : solution) : solution =
    let candidateReductions = find p solution.current in
    step p solution candidateReductions
