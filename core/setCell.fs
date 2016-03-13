module core.setCell

open sudoku
open puzzlemap
open hints

let setCellDigitApply (p : puzzleMap) (value : value) (current : current) : current = 

    let update (cell : cell) : cellContents =
        let cellContents = current.Get cell
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells =
                p.cellHouseCells.Get value.cell

            if value.cell = cell then BigNumber value.digit
            else if Cells.contains cell cells then 
                PencilMarks (Digits.remove value.digit candidates)
            else cellContents

    makeMapLookup<cell, cellContents> p.cells update
    :> current

type setCellDigitError = 
    { cell : cell
      candidate : digit
      digit : digit }

let setCellDigitTry (cell : cell) (candidate : digit) (cellCandidates : cellCandidates) : value option = 
    let candidates = cellCandidates.Get cell

    if Digits.contains candidate candidates then
        makeValue cell candidate
        |> Some
    else None

let setCellHintDescription (p : puzzleMap) (setCellValue : value) : hintDescription =
    { hintDescription.primaryHouses = Houses.empty
      secondaryHouses = Houses.empty
      candidateReductions = CandidateReductions.empty
      setCellValueAction = Some setCellValue
      pointers = CandidateReductions.empty
      focus = Digits.empty }

let setCellStep (p : puzzleMap) (setCellValue : value) (solution : solution) : solution =
    { solution with current = setCellDigitApply p setCellValue solution.current
                    steps = (Placement setCellValue) :: solution.steps }
