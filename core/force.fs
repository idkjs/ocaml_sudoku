module core.force

open System
open System.Diagnostics

open setCell
open sudoku
open puzzlemap

let isPencilMarksCellContents (cellContents : CellContents) : bool =
    match cellContents with
    | BigNumber _ -> false
    | PencilMarks _ -> true

let isValidCellContents (cellContents : CellContents) : bool =
    match cellContents with
    | BigNumber _ -> true
    | PencilMarks candidates -> Set.count candidates > 0

let cellCellContents (solution : Solution) (cell : Cell) : CellContents =
    solution.current.Item cell

let isValid (solution : Solution) (cells : Cell list) : bool =
    cells
    |> List.map (cellCellContents solution)
    |> List.forall isValidCellContents

let rec searchr (solution : Solution) (cells : Cell list) (puzzleHouseCellCells : MapCellHouseCells) (existing : Solution list) : Solution list = 
    let emptyCell : Cell option =
        cells
        |> List.tryFind ((cellCellContents solution) >> isPencilMarksCellContents)

    match emptyCell with
    | Some cell ->
        let candidates =
            let cellContents = solution.current.Item cell
            match cellContents with
            | BigNumber _ -> []
            | PencilMarks candidates -> candidates |> Set.toList

        List.collect
            (fun digit ->
                let setCellValue = makeSetCellDigit cell digit
                
                let current = setCellDigitApply puzzleHouseCellCells setCellValue solution.current

                let newSolution =
                    { solution with
                        current = current
                        steps = (Placement setCellValue) :: solution.steps }

                //Console.WriteLine ("Trying {0}", setCellValue)

                if isValid newSolution cells then
                    //Console.WriteLine(">")
                    searchr newSolution cells puzzleHouseCellCells existing
                else
                    (*
                    let cell =
                        List.find
                            (fun cell -> 
                                let cellContents = newSolution.current cell
                                match cellContents with
                                | BigNumber _ -> false
                                | PencilMarks candidates -> Set.count candidates = 0)
                            cells

                    Console.WriteLine(String.Format("< {0}", cell))
                    *)
                    [])
            candidates

    | None -> solution :: existing

let solve (solution : Solution) (cells : Cell list) (puzzleHouseCellCells : MapCellHouseCells) : Solution list =
    let stopwatch = new Stopwatch()
    stopwatch.Start()

    let results = searchr solution cells puzzleHouseCellCells []

    stopwatch.Stop()
    Console.WriteLine("Time elapsed: {0}", stopwatch.Elapsed)

    results
