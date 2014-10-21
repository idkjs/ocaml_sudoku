module core.force

open System
open System.Diagnostics

open setCell
open sudoku

let isValid (solution : Solution) (cells : Cell list) =
    List.forall
        (fun cell ->
            let cellContents = solution.current cell
            match cellContents with
            | BigNumber _ -> true
            | PencilMarks candidates -> Set.count candidates > 0)
        cells

let rec searchr (solution : Solution) (cells : Cell list) (puzzleHouseCellCells : Cell -> Set<Cell>) (existing : Solution list) : Solution list = 
    let emptyCell =
        List.tryFind
            (fun cell ->
                let cellContents = solution.current cell
                match cellContents with
                | BigNumber _ -> false
                | PencilMarks _ -> true)
            cells

    match emptyCell with
    | Some cell ->
        let candidates =
            let cellContents = solution.current cell
            match cellContents with
            | BigNumber _ -> []
            | PencilMarks candidates -> candidates |> Set.toList

        List.collect
            (fun digit ->
                let setCellValue = makeSetCellDigit cell digit
                
                let newSolution =
                    { solution with
                        current = setCellDigitApply puzzleHouseCellCells setCellValue solution.current
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

let solve (solution : Solution) (cells : Cell list) (puzzleHouseCellCells : Cell -> Set<Cell>) : Solution list =
    searchr solution cells puzzleHouseCellCells []
