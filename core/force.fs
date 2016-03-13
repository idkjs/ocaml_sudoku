module core.force

open System
open System.Diagnostics

open sudoku
open puzzlemap
open setCell

let isPencilMarksCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> false
    | PencilMarks _ -> true

let isValidCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> true
    | PencilMarks candidates -> Digits.count candidates > 0

let isValid (solution : solution) (cells : cell array) : bool =
    cells
    |> Array.map solution.current.Get
    |> Array.forall isValidCellContents

let rec searchr (p : puzzleMap) (solution : solution) (existing : solution array) : solution array = 
    let emptyCell : cell option =
        p.cells
        |> Array.tryFind (solution.current.Get >> isPencilMarksCellContents)

    match emptyCell with
    | Some cell ->
        let candidates =
            let cellContents = solution.current.Get cell
            match cellContents with
            | BigNumber _ -> Array.empty
            | PencilMarks candidates -> candidates |> Digits.toArray
        
        candidates
        |> Array.map
            (fun digit ->
                let setCellValue = makeValue cell digit
                
                let current = setCellDigitApply p setCellValue solution.current

                let newSolution =
                    { solution with
                        current = current
                        steps = (Placement setCellValue) :: solution.steps }

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
                    Array.empty)
            |> Array.concat
    | None -> Array.append existing [|solution|]

let solve (p : puzzleMap) (solution : solution) : solution array =
    let stopwatch = new Stopwatch()
    stopwatch.Start()

    let results = searchr p solution Array.empty

    stopwatch.Stop()
    Console.WriteLine("Time elapsed: {0}", stopwatch.Elapsed)

    results
