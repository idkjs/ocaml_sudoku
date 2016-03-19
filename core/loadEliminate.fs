module core.loadEliminate

open smap
open sudoku
open puzzlemap
open hints

let loadEliminateFind  (p : puzzleMap) (current : current) : candidateReductions = 

    let reductions (cell : cell) : digits option =
        let cellContents = SMap.get current cell
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                cell
                |> SMap.get p.cellHouseCells
                |> Cells.choose
                    (fun cell ->
                        let houseCellContents = SMap.get current cell
                        match houseCellContents with
                        | BigNumber digit -> Some digit
                        | PencilMarks _ -> None)
                |> Digits.ofSet

            if Digits.count digits > 0 then Some digits
            else None

    p.cells
    |> List.choose
        (fun cell ->
            reductions cell
            |> Option.map (fun digits -> makeCandidateReduction cell digits))
    |> CandidateReductions.ofList

let loadEliminateApply (p : puzzleMap) (candidateReductions : candidateReductions) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
        |> CandidateReductions.toList
        |> List.map (fun cr -> (cr.cell, cr.candidates))
        |> Map.ofSeq

    let update (cell : cell) : cellContents =
        let cellContents = SMap.get current cell
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates ->
            let digitsOpt = candidateReductionsLookup.TryFind cell
            match digitsOpt with
            | Some digits ->
                Digits.difference candidates digits
                |> PencilMarks
            | None -> cellContents

    SMap.ofLookup<cell, cellContents> p.cells update

let loadEliminateDescription (p : puzzleMap) (candidateReductions : candidateReductions) : hintDescription =
    { hintDescription.primaryHouses = Houses.empty
      secondaryHouses = Houses.empty
      candidateReductions = candidateReductions
      setCellValueAction = None
      pointers = CandidateReductions.empty
      focus = Digits.empty }

let loadEliminateStep (p : puzzleMap) (solution : solution) (candidateReductions : candidateReductions) : solution =

    { solution with current = loadEliminateApply p candidateReductions solution.current
                    steps = LoadEliminate :: solution.steps }

let loadEliminateFindAndApply (p : puzzleMap) (solution : solution) : solution =
    let candidateReductions = loadEliminateFind p solution.current
    loadEliminateStep p solution candidateReductions
