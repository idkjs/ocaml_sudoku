open Smap
open Sudoku
open Puzzlemap
open Hints

let loadEliminateFind  (p : puzzleMap) (current : current) : candidateReduction list = 

    let reductions (cell : cell) : digits option =
        let cellContents = SMap.get current cell in
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                cell
                |> SMap.get p.cellHouseCells
                |> Cells.choose
                    (fun cell ->
                        let houseCellContents = SMap.get current cell in
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
            |> Option.map (fun digits -> makeCandidateReduction cell digits))

let loadEliminateApply (p : puzzleMap) (candidateReductions : candidateReduction list) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
        |> List.map (fun cr -> (cr.cell, cr.candidates))
        |> Map.ofSeq
        in

    let update (cell : cell) : cellContents =
        let cellContents = SMap.get current cell in
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

    SMap.ofLookup<cell, cellContents> (Cells.toList p.cells) update

let loadEliminateDescription (p : puzzleMap) (candidateReductions : candidateReduction list) : hintDescription =
    { hintDescription.primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = candidateReductions;
      setCellValueAction = None;
      pointers = [];
      focus = Digits.empty }

let loadEliminateStep (p : puzzleMap) (solution : solution) (candidateReductions : candidateReduction list) : solution =
    { solution with current = loadEliminateApply p candidateReductions solution.current;
                    steps = LoadEliminate :: solution.steps }

let loadEliminateFindAndApply (p : puzzleMap) (solution : solution) : solution =
    let candidateReductions = loadEliminateFind p solution.current in
    loadEliminateStep p solution candidateReductions
