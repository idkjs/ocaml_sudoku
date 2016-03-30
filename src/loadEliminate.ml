open Sudoku
open Puzzlemap

let find  (p : puzzleMap) (current : current) : candidateReduction list = 

    let reductions (cell : cell) : digits option =
        let cellContents = Current.get current cell in
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                cell
                |> Smap.get p.cellHouseCells
                |> Cells.choose
                    (fun cell ->
                        let houseCellContents = Current.get current cell in
                        match houseCellContents with
                        | BigNumber digit -> Some digit
                        | PencilMarks _ -> None)
                |> Digits.ofList in

            if Digits.count digits > 0 then Some digits
            else None
        in

    p.cells
    |> Cells.toList
    |> List.choose
        (fun cell ->
            reductions cell
            |> Option.map (fun digits -> CandidateReduction.make cell digits))

let apply (p : puzzleMap) (candidateReductions : candidateReduction list) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
        |> List.map (fun cr -> (cr.cell, cr.candidates))
        |> Map.ofSeq
        in

    let update (cell : cell) : cellContents =
        let cellContents = Current.get current cell in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates ->
            let digitsOpt = candidateReductionsLookup.TryFind cell in
            match digitsOpt with
            | Some digits ->
                Digits.difference candidates digits
                |> PencilMarks
            | None -> cellContents
        in

    Smap.ofLookup<cell, cellContents> (Cells.toList p.cells) update
    |> Current

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
