module core.setCell

open sudoku
open puzzlemap
open hints

let setCellDigitApply (cellHouseCells : cellHouseCells) (setCellValue : value) : current -> current = 

    let update (cell : cell) (cellContents : cellContents) : cellContents =
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = cellHouseCells.Get setCellValue.cell

            if setCellValue.cell = cell then BigNumber setCellValue.digit
            else if Set.contains cell cells then 
                PencilMarks(Set.remove setCellValue.digit candidates)
            else cellContents

    Map.map update

type setCellDigitError = 
    { cell : cell
      candidate : digit
      digit : digit }

let setCellDigitTry (cell : cell) (candidate : digit) (cellCandidates : cellCandidates) : value option = 
    let candidates = cellCandidates.Get cell

    if Set.contains candidate candidates then
        makeSetCellDigit cell candidate
        |> Some
    else None

let setCellHintDescription (p : puzzleMap) (setCellValue : value) : hintDescription =
    let hd = 
        { hintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set []
          setCellValueAction = Some setCellValue
          pointers = set []
          focus = set [] }
                
    hd

let setCellStep (p : puzzleMap) (setCellValue : value) (solution : solution) : solution =
    { solution with current = setCellDigitApply p.cellHouseCells setCellValue solution.current
                    steps = (Placement setCellValue) :: solution.steps }
