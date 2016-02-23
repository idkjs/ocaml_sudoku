module core.setCell

open sudoku
open puzzlemap
open hints

let setCellDigitApply (cellHouseCells : CellHouseCells) (setCellValue : Value) : Current -> Current = 

    let update (cell : Cell) (cellContents : CellContents) : CellContents =
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = cellHouseCells.Get setCellValue.cell

            if setCellValue.cell = cell then BigNumber setCellValue.digit
            else if Set.contains cell cells then 
                PencilMarks(Set.remove setCellValue.digit candidates)
            else cellContents

    Map.map update

type SetCellDigitError = 
    { cell : Cell
      candidate : Digit
      digit : Digit }

let setCellDigitTry (cell : Cell) (candidate : Digit) (cellCandidates : CellCandidates) : Value option = 
    let candidates = cellCandidates.Get cell

    if Set.contains candidate candidates then
        makeSetCellDigit cell candidate
        |> Some
    else None

let setCellHintDescription (p : PuzzleMap) (setCellValue : Value) : HintDescription2 =
    let hd = 
        { HintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set []
          setCellValueAction = Some setCellValue
          pointers = set [] }
                
    mhas p.cells p.cellHouseCells p.houseCells hd

let setCellStep (p : PuzzleMap) (setCellValue : Value) (solution : Solution) : Solution =
    { solution with current = setCellDigitApply p.cellHouseCells setCellValue solution.current
                    steps = (Placement setCellValue) :: solution.steps }
