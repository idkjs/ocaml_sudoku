module main

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console
open setCell
open fullHouse
open hiddenSingle
open nakedSingle
open nakedPair
open command
open shell
open tactics

let symbolToEntry (puzzleMaps:PuzzleMaps) (symbolLookup:SymbolLookup) =
    fun (cell:Cell) ->
        match symbolLookup cell with
        | Some(e) -> Given(e)
        | None ->
            let symbols = houseCellsForCell puzzleMaps cell symbolLookup
            let candidates = Set.map symbolToCandidate symbols

            Candidates (fun candidate -> if Set.contains candidate candidates then Excluded else Possible)

type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write:ConsoleCharWriter) (grid:EntryLookup) (action:Action) (puzzleMaps:PuzzleMaps) =
    Seq.iter write (printGrid defaultGridChars sNL (grid >> entryToConsole >> drawF) puzzleMaps)

    match action with
    | SetValue { SetCellValue.cell = {col = col; row = row} as cell; value = Candidate s} -> write (CStr (String.Format("SetValue: {0} = {1}", formatCell cell, s)))
    | ClearCandidate({col = col; row = row} as cell, Symbol s) -> write (CStr (String.Format("ClearCandidate: {0} = {1}", formatCell cell, s)))

    write NL

let print_last solution (puzzleMaps:PuzzleMaps) =
    match solution.steps with
    | s :: _ -> print_step ConsoleWriteChar solution.grid s puzzleMaps
    | [] -> ()


let parse (item:string) (alphabet:Candidate list) solution (puzzleMaps:PuzzleMaps) : Solution =
    let draw_grid (dr:'a->ConsoleChar) (gridTo:Cell->'a) = Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (gridTo >> dr) puzzleMaps)

    let draw_cell (symbol:Candidate) = drawFLFE (List.nth alphabet ((List.length alphabet) / 2)) symbol
    let draw_cell2 (symbol:Candidate) = drawFL2 (List.nth alphabet ((List.length alphabet) / 2)) symbol
    let draw_full (dr:Candidate->'a->ConsoleChar) (symbolTo:Cell->Candidate->'a) = Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL symbolTo puzzleMaps alphabet dr)

    let print_g cell = solution.grid cell |> entryToConsole
    let print_grid = entryAndCandidateToConsole solution.grid

    let alphaset = alphabet |> Set.ofList

    Console.WriteLine item

    if item = "print" then
        draw_full draw_cell print_grid
        solution

    else if item.StartsWith "set" then
        let set = ui_set item alphabet solution.grid puzzleMaps
        let newSolution =
            match set with
            | Some setCellValue ->
                let print_grid2 = setCellCandidateGrid setCellValue puzzleMaps (solution.grid >> getCandidateEntries alphaset) print_grid
                draw_full draw_cell2 print_grid2

                { solution with
                    grid = setACell setCellValue puzzleMaps (solution.grid >> getCandidateEntries alphaset) solution.grid
                    steps = (SetValue setCellValue) :: solution.steps }
            | None ->
                Console.WriteLine("")
                solution

        print_last newSolution puzzleMaps

        match set with
        | Some setCellValue ->
            let print_grid2 = setCellCandidateGrid setCellValue puzzleMaps (solution.grid >> getCandidateEntries alphaset) print_grid
            draw_full draw_cell2 print_grid2
        | _ -> ()

        newSolution

    else if item = "fh" then
        let lastGrid = solution.grid

        let hints = findFullHouse (solution.grid >> getCandidateEntries alphaset) puzzleMaps

        List.iter (
            fun hint ->
                Console.WriteLine (printFullHouse hint)

                let st = fullHouseSymbolTo hint solution.grid
                draw_grid drawFL st

                let print_grid2 = fullHouseFullSymbolTo hint print_grid
                draw_full draw_cell2 print_grid2
            )
            hints
        solution

    else if item = "hs" then
        let lastGrid = solution.grid

        let hints = findHiddenSingles alphabet (solution.grid >> getCandidateEntries alphaset) puzzleMaps

        List.iter (
            fun hint ->
                Console.WriteLine (formatHiddenSingle hint)

                let st = hiddenSingleSymbolTo hint print_g
                draw_grid drawFL st
            )
            hints
        solution

    else if item = "ns" then
        let lastGrid = solution.grid

        let hints = findNakedSingles (solution.grid >> getCandidateEntries alphaset) puzzleMaps.cells

        List.iter (
            fun hint ->
                Console.WriteLine (printNakedSingle hint)

                let st = nakedSingleSymbolTo hint print_g
                draw_grid drawFL st

                let print_grid2 = nakedSingleFullSymbolTo hint print_grid

                draw_full draw_cell2 print_grid2
            )
            hints
        solution

    else if item = "np" then
        let lastGrid = solution.grid

        let hints = findNakedPairs (solution.grid >> getCandidateEntries alphaset) puzzleMaps

        List.iter (
            fun hint ->
                printNakedPair hint

                let print_grid2 = nakedPairSymbolTo hint print_grid
                draw_full draw_cell2 print_grid2
            )
            hints
             
        solution

    else
        solution

let run (candidates:Candidate list) (solution:Solution ref) (puzzleMaps:PuzzleMaps) item =
    if item = "quit"
        then
            Some(item)
        else
            solution := parse item candidates !solution puzzleMaps
            None

let makePuzzleMaps (puzzleSpec : Puzzle) =
    let length = puzzleSpec.alphabet.Length

    let columns = columns length
    let rows = rows length
    let cells = allCells length

    let columnCells = columnCells length
    let rowCells = rowCells length


    let (cellBoxLookup, boxCellLookup) = makeContainerBoxCell (boxes puzzleSpec.boxWidth puzzleSpec.boxHeight length) cells puzzleSpec.boxWidth puzzleSpec.boxHeight

    let houseCells =
        {
            columns = columns
            rows = rows
            cells = cells

            cellColumn = cellColumn
            columnCells = columnCells
            cellRow = cellRow
            rowCells = rowCells
            cellBox = cellBoxLookup
            boxCells = boxCellLookup

            stacks = stacks puzzleSpec.boxWidth length
            bands = bands puzzleSpec.boxHeight length
            boxes = boxes puzzleSpec.boxWidth puzzleSpec.boxHeight length

            columnStack = columnStack puzzleSpec.boxWidth
            stackColumns = stackColumns puzzleSpec.boxWidth
            rowBand = rowBand puzzleSpec.boxHeight
            bandRows = bandRows puzzleSpec.boxHeight
        }

    houseCells


let mainWriter = ConsoleWriteChar

let repl (sudoku:string) (puzzleSpec : Puzzle) =

    Console.WriteLine sudoku

    let alphabetisedLine = loadLine sudoku puzzleSpec.alphabet

    let puzzleMaps = makePuzzleMaps puzzleSpec

    let puzzleGrid = loadPuzzle alphabetisedLine puzzleMaps.cells

    let alphaset = puzzleSpec.alphabet |> Set.ofList

    let candidates = List.map symbolToCandidate puzzleSpec.alphabet

    let candidateSet = Set.ofList candidates

    let stoe = symbolToEntry puzzleMaps puzzleGrid

    let solutionGrid = flattenEntry stoe puzzleMaps.cells

    let solution = ref ({ grid = solutionGrid; steps = [] })

    let line = List.foldBack (puzzleGrid >> symbolOptionToConsoleChar >> drawF >> cons) puzzleMaps.cells [NL]
    List.iter mainWriter line
    mainWriter NL

    let prows = printRowOnOneLine (puzzleGrid >> symbolOptionToConsoleChar >> drawF) puzzleMaps.rowCells sNL puzzleMaps.rows
    Seq.iter mainWriter prows

    Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (puzzleGrid >> symbolOptionToConsoleChar >> drawF) puzzleMaps)

    Seq.tryPick (run candidates solution puzzleMaps) readlines |> ignore


let defaultPuzzleSpec = {
    boxWidth = 3 * 1<width>
    boxHeight = 3 * 1<height>
    alphabet = [ for i in 1 .. 9 -> (char) i + '0' |> Symbol ]
    symbols = fun _ -> None
}

(*
let defaultPuzzleSpec = {
    boxWidth = 4 * 1<width>
    boxHeight = 2 * 1<height>
    alphabet = [ for i in 1 .. 8 -> (char) i + '0' |> Symbol ]
    symbols = fun _ -> None
}
*)
// Input puzzle
Console.WriteLine "1........2........3........4........5........6........7........8........9........"
Console.WriteLine "123456789123456789123456789123456789123456789123456789123456789123456789123456789"
Console.WriteLine "800739006370465000040182009000600040054300610060500000400853070000271064100940002"

//let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800"
//let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000"

// FullHouse
let example = "800739006370465000040182009000600040054300610060500000400853070000271064100940002"

repl example defaultPuzzleSpec

Console.WriteLine "bye"
