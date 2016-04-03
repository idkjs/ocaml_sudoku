open Sudoku
open Puzzlemap

(*F# open FSharp.Compatibility.OCaml F#*)

let isPencilMarksCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> false
    | PencilMarks _ -> true

let isValidCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> true
    | PencilMarks candidates -> Digits.count candidates > 0

let isValid (solution : solution) (cells : cells) : bool =
    cells
    |> Cells.toList
    |> List.map (Current.get solution.current)
    |> List.for_all isValidCellContents

let rec searchr (p : puzzleMap) (solution : solution) (existing : solution list) : solution list =
    let emptyCell : cell option =
        let is_cell_empty (cell : cell) : bool =
            Current.get solution.current cell |> isPencilMarksCellContents
            in

        let cell_list = Cells.toList p.cells in

        if List.exists is_cell_empty cell_list then
            Some (List.find is_cell_empty cell_list)
        else None
        in

    match emptyCell with
    | Some cell ->
        let candidates =
            let cellContents = Current.get solution.current cell in
            match cellContents with
            | BigNumber _ -> []
            | PencilMarks candidates -> candidates |> Digits.toList
            in

        candidates
        |> List.map
            (fun digit ->
                let setCellValue = Value.make cell digit in
                
                let current = SetCell.apply p setCellValue solution.current in

                let newSolution =
                    { solution with
                        current = current;
                        steps = (Placement setCellValue) :: solution.steps }
                    in

                (*Console.WriteLine ("Trying {0}", setCellValue) *)

                if isValid newSolution p.cells then
                    (*Console.WriteLine(">")*)
                    searchr p newSolution existing
                else
                    (*
                    let cell =
                        List.find
                            (fun cell -> 
                                let cellContents = newSolution.current cell
                                match cellContents with
                                | BigNumber _ -> false
                                | PencilMarks candidates -> Digits.count candidates = 0)
                            cells

                    Console.WriteLine(String.Format("< {0}", cell))
                    *)
                    [])
            |> List.concat
    | None -> solution :: existing

let solve (p : puzzleMap) (solution : solution) : solution list =
    searchr p solution []
