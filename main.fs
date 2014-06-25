module main

open System
open System.Text

open sudoku
open puzzlemap
open format
open tactics
open fullHouse
open hiddenSingle
open nakedSingle
open nakedPair

open hints
open console
open command
open shell

let symbolToEntry (symbolLookup:SymbolLookup) (puzzleMaps:PuzzleMaps) (alphaset:Set<Symbol>) cell =
    match symbolLookup cell with
    | Some(e) -> Given(e)
    | None ->
        let cs = houseCellsForCell puzzleMaps cell symbolLookup
        Set.difference alphaset cs |> Candidates

type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write:ConsoleCharWriter) (grid:EntryLookup) (action:Action) (puzzleMaps:PuzzleMaps) =
    Seq.iter write (printGrid defaultGridChars sNL (grid >> entryToConsole >> drawFL) puzzleMaps)

    match action with
    | SetValue({col = col; row = row} as cell, Symbol s) -> write (CStr (String.Format("SetValue: {0} = {1}", formatCell cell, s)))
    | ClearCandidate({col = col; row = row} as cell, Symbol s) -> write (CStr (String.Format("ClearCandidate: {0} = {1}", formatCell cell, s)))

    write NL

let print_last solution (puzzleMaps:PuzzleMaps) =
    match solution.steps with
    | s :: _ -> print_step ConsoleWriteChar solution.grid s puzzleMaps
    | [] -> ()


let parse (item:string) (alphabet:Alphabet) solution (puzzleMaps:PuzzleMaps) : Solution = 
    Console.WriteLine item

    if item = "print" then
        let grid = solution.grid
        let print_grid cell symbol = grid cell |> entryAndCandidateToConsole symbol
        let draw_cell symbol = drawFL2 (List.nth alphabet ((List.length alphabet) / 2)) symbol

        let pf = print_full defaultSolutionChars sNL print_grid puzzleMaps alphabet draw_cell
        Seq.iter ConsoleWriteChar pf
        solution

    else if item.StartsWith "set" then
        let lastGrid = solution.grid

        let set = ui_set item alphabet lastGrid puzzleMaps
        let newSolution =
            match set with
            | Some (grid, action) -> { solution with grid = grid; steps = action :: solution.steps }
            | None ->
                Console.WriteLine("")
                solution

        print_last newSolution puzzleMaps
        newSolution

    else if item = "fh" then
        let lastGrid = solution.grid

        let hints = findFullHouse (lastGrid >> getCandidateEntries) puzzleMaps

        List.iter (
            fun hint ->
                Console.WriteLine (printFullHouse hint)

                let st cell = lastGrid cell |> fullHouseSymbolTo hint entryToConsole cell |> drawFL
                Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL st puzzleMaps)

                let print_grid cell symbol = lastGrid cell |> fullHouseFullSymbolTo hint entryAndCandidateToConsole cell symbol
                let draw_cell symbol = drawFL2 (List.nth alphabet ((List.length alphabet) / 2)) symbol

                Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL print_grid puzzleMaps alphabet draw_cell)
                )

            hints
        solution

    else if item = "hs" then
        let lastGrid = solution.grid

        let hints = findHiddenSingles alphabet (lastGrid >> getCandidateEntries) puzzleMaps

        List.iter (
            fun hint ->
                Console.WriteLine (formatHiddenSingle hint)

                let st cell = lastGrid cell |> hiddenSingleSymbolTo hint entryToConsole cell |> drawFL
                Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL st puzzleMaps))
            hints
        solution

    else if item = "ns" then
        let lastGrid = solution.grid

        let hints = findNakedSingles (lastGrid >> getCandidateEntries) puzzleMaps.cells

        List.iter (
            fun hint ->
                Console.WriteLine (printNakedSingle hint)

                let st cell = lastGrid cell |> nakedSingleSymbolTo hint entryToConsole cell |> drawFL
                Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL st puzzleMaps)

                let print_grid cell symbol = lastGrid cell |> nakedSingleFullSymbolTo hint entryAndCandidateToConsole cell symbol
                let draw_cell symbol = drawFL2 (List.nth alphabet ((List.length alphabet) / 2)) symbol

                Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL print_grid puzzleMaps alphabet draw_cell))
            hints
        solution

    else if item = "np" then
        let lastGrid = solution.grid

        let hints = findNakedPairs (lastGrid >> getCandidateEntries) puzzleMaps

        List.iter printNakedPair hints

        List.iter (
            fun hint ->
                let print_grid cell symbol = lastGrid cell |> nakedPairSymbolTo hint entryAndCandidateToConsole cell symbol
                let draw_cell symbol = drawFL2 (List.nth alphabet ((List.length alphabet) / 2)) symbol
                Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL print_grid puzzleMaps alphabet draw_cell))
            hints

        solution

    else
        solution

let run alphabet (solution:Solution ref) (puzzleMaps:PuzzleMaps) item =
    if item = "quit"
        then
            Some(item)
        else
            solution := parse item alphabet !solution puzzleMaps
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

    let stoe = symbolToEntry puzzleGrid puzzleMaps alphaset

    let solutionGrid = flattenEntry stoe puzzleMaps.cells

    let solution = ref ({ grid = solutionGrid; steps = [] })

    let line = List.foldBack (puzzleGrid >> symbolOptionToConsoleChar >> drawFL >> cons) puzzleMaps.cells [NL]
    List.iter mainWriter line
    mainWriter NL

    let prows = printRowOnOneLine (puzzleGrid >> symbolOptionToConsoleChar >> drawFL) puzzleMaps.rowCells sNL puzzleMaps.rows
    Seq.iter mainWriter prows

    Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (puzzleGrid >> symbolOptionToConsoleChar >> drawFL) puzzleMaps)

    Seq.tryPick (run puzzleSpec.alphabet solution puzzleMaps) readlines |> ignore


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
