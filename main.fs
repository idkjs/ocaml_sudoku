#light

module main

open System
open System.Text

open sudoku
open format
open tactics
open hints
open console
open command
open shell

let symbolsToAllCandidates alphabet symbolOpt =
    match symbolOpt with
    | Some(e) -> Given(e)
    | None -> Candidates(alphabet |> Set.ofList)


let beginSolution (symbolLookup : SymbolLookup) houseCells (cells:Cell list) (puzzle : Puzzle) =
    let s = 
        List.map (
            fun cell ->
                let e1 = symbolsToAllCandidates puzzle.alphabet (symbolLookup cell)
                let entry = symbolsToCandidates symbolLookup houseCells e1 cell
                (cell, entry)
        ) cells

    let s2 = s |> Map.ofList

    let solutionGrid = new System.Collections.Generic.Dictionary<Cell, Entry>(s2)

    let firstStep = { grid = (solutionGridCellLookup solutionGrid); action = Load }

    let solution = { puzzle = puzzle; steps = [firstStep] }

    solution



type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write:ConsoleCharWriter) step stacks stackColumns bands bandRows =
    let grid = step.grid
    Seq.iter write (print_long defaultGridChars (grid >> entryToConsole) stacks stackColumns bands bandRows)

    match step.action with
    | Load -> write (CStr "Load")
    | SetValue({col = col; row = row}, Symbol s) -> write (CStr (String.Format("SetValue: ({0}, {1}) = {2}", (int)col.col, (int)row.row, s)))
    | ClearCandidate({col = col; row = row}, Symbol s) -> write (CStr (String.Format("ClearCandidate: ({0}, {1}) = {2}", (int)col.col, (int)row.row, s)))

    write NL

let print_last solution (stacks:Stack list) stackColumns (bands:Band list) bandRows =
    let lastStep = solution.steps.Head
    print_step ConsoleWriteChar lastStep stacks stackColumns bands bandRows



let parse (item:string) gridSize alphabet solution (stacks:Stack list) (stackColumns:Stack -> Column list) (bands:Band list) (bandRows:Band->Row list) houseCells (cells:Cell list) (columns:Column list) (rows:Row list) (boxes:Box list) : Solution = 
    Console.WriteLine item

    if item = "print" then
        let lastStep = solution.steps.Head
        let grid = lastStep.grid
        let print_grid cell symbol = entryAndCandidateToConsole (List.nth alphabet ((List.length alphabet) / 2))  symbol (grid cell)

        let pf = print_full defaultSolutionChars print_grid stacks stackColumns bands bandRows alphabet
        Seq.iter ConsoleWriteChar pf
        solution

    else if item.StartsWith "set" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let set = ui_set item gridSize alphabet lastGrid houseCells cells
        let newSolution =
            match set with
            | Some step -> { solution with steps = step :: solution.steps }
            | None ->
                Console.WriteLine("")
                solution

        print_last newSolution stacks stackColumns bands bandRows
        newSolution

    else if item = "fh" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let hints = findFullHouse lastGrid columns rows boxes houseCells

        printFullHouse hints

        List.iter (
            fun hint ->
                Seq.iter ConsoleWriteChar (print_long defaultGridChars (fullHouseSymbolTo lastGrid houseCells hint) stacks stackColumns bands bandRows))
            hints
        solution

    else if item = "hs" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let hints = findHiddenSingles alphabet lastGrid columns rows boxes houseCells

        printHiddenSingles hints

        List.iter (
            fun hint ->
                Seq.iter ConsoleWriteChar (print_long defaultGridChars (hiddenSingleSymbolTo lastGrid houseCells hint) stacks stackColumns bands bandRows))
            hints
        solution

    else if item = "ns" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let hints = findNakedSingles lastGrid cells

        printNakedSingles hints

        List.iter (
            fun hint ->
                Seq.iter ConsoleWriteChar (print_long defaultGridChars (nakedSingleSymbolTo lastGrid houseCells hint) stacks stackColumns bands bandRows))
            hints
        solution

    else if item = "np" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let hints = findNakedPairs lastGrid columns rows boxes houseCells

        printNakedPairs hints

        solution

    else
        solution
(*
        List.iter (
            fun hint ->
                List.iter ConsoleWriteChar (print_full (nakedPairSymbolTo lastGrid houseCells hint) stacks stackColumns bands bandRows alphabet []))
            hints
*)

let run gridSize alphabet (solution:Solution ref) (stacks:Stack list) stackColumns (bands:Band list) bandRows houseCells (cells:Cell list) (columns:Column list) (rows:Row list) (boxes:Box list) item =
    if item = "quit"
        then
            Some(item)
        else
            solution := parse item gridSize alphabet !solution stacks stackColumns bands bandRows houseCells cells columns rows boxes
            None

[<NoEquality; NoComparison>]
type Tactics = {
    stacks : Stack list
    bands : Band list

    columns : Column list
    rows : Row list
    boxes : Box list

    cells : Cell list

    stackColumns : Stack -> Column list
    bandRows : Band -> Row list

    columnCells : Column -> Cell list
    rowCells : Row -> Cell list
    boxCells : Box -> Cell list

    houseCells : HouseCells
}

let makeTactics (puzzleSpec : Puzzle) =
    let (stacks, columns, columnStackLookup, stackColumnLookup) = eachStackColumns puzzleSpec.boxWidth puzzleSpec.alphabet.Length
    let (bands, rows, rowBandLookup, bandRowLookup) = eachBandRows puzzleSpec.boxHeight puzzleSpec.alphabet.Length
    let boxes = eachBox stacks bands
    let cells = allCells columns rows
    let (columnCells, cellColumnLookup) = makeContainerColumnCell columns cells
    let (rowCells, cellRowLookup) = makeContainerRowCell rows cells
    let (cellBoxLookup, boxCellLookup) = makeContainerBoxCell boxes cells columnStackLookup rowBandLookup

    let houseCells =
        {
            HouseCells.cellColumn = cellColumnLookup
            columnCells = columnCells
            cellRow = cellRowLookup
            rowCells = rowCells
            cellBox = cellBoxLookup
            boxCells = boxCellLookup
        }

    let tactics =
        {
            stacks = stacks
            columns = columns
            bands = bands
            rows = rows
            boxes = boxes
            cells = cells

            stackColumns = stackColumnLookup
            bandRows = bandRowLookup

            columnCells = columnCells
            rowCells = rowCells
            boxCells = boxCellLookup

            houseCells = houseCells
        }

    tactics


let mainWriter = ConsoleWriteChar

let repl (sudoku:string) (puzzleSpec : Puzzle) =

    Console.WriteLine sudoku

    let alphabetisedLine = loadLine sudoku puzzleSpec.alphabet

    let tactics = makeTactics puzzleSpec

    let puzzleGrid = loadPuzzle alphabetisedLine tactics.cells

    let solution = ref (beginSolution puzzleGrid tactics.houseCells tactics.cells puzzleSpec)

    let line = List.foldBack (puzzleGrid >> symbolOptionToConsoleChar >> cons) tactics.cells [NL]
    List.iter mainWriter line
    mainWriter NL

    let rows = printRowOnOneLine (puzzleGrid >> symbolOptionToConsoleChar) tactics.houseCells.rowCells sNL tactics.rows
    Seq.iter mainWriter rows

    Seq.iter ConsoleWriteChar (print_long defaultGridChars (puzzleGrid >> symbolOptionToConsoleChar) tactics.stacks tactics.stackColumns tactics.bands tactics.bandRows)

    Seq.tryPick (run puzzleSpec.alphabet.Length puzzleSpec.alphabet solution tactics.stacks tactics.stackColumns tactics.bands tactics.bandRows tactics.houseCells tactics.cells tactics.columns tactics.rows tactics.boxes) readlines |> ignore


let defaultPuzzleSpec = {
    boxWidth = 3
    boxHeight = 3
    alphabet = [ for i in 1 .. 9 -> (char) i + '0' |> Symbol ]
    symbols = fun _ -> None
}

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
