module main

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console
open setCell
open clearCandidate
open fullHouse
open hiddenSingle
open nakedSingle
open nakedPair
open command
open shell
open tactics

open System.Diagnostics
open System.Runtime.InteropServices;

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize () =
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

let symbolToEntry (puzzleMaps:PuzzleMaps) (symbolLookup:Cell->Symbol option) =
    fun (cell:Cell) ->
        match symbolLookup cell with
        | Some(e) -> Given(e)
        | None ->
            let symbols = houseCellsForCell puzzleMaps cell symbolLookup
            let candidates = Set.map symbolToCandidate symbols

            Candidates (fun candidate -> if Set.contains candidate candidates then Excluded else Possible)

type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write:ConsoleCharWriter) (grid:Cell->AnnotatedSymbol<AnnotatedCandidate>) (action:Action) (puzzleMaps:PuzzleMaps) =
    Seq.iter write (printGrid defaultGridChars sNL (grid >> drawAnnotatedSymbol) puzzleMaps)

    match action with
    | SetValue sv -> write (CStr (setValueToString sv))
    | ClearCandidate cc -> write (CStr (clearCandidateToString cc))
    | ApplyHint ht -> ()

    write NL

let print_last solution (puzzleMaps:PuzzleMaps) =
    match solution.steps with
    | s :: _ -> print_step ConsoleWriteChar solution.grid s puzzleMaps
    | [] -> ()


let parse (item:string) (alphabet:Candidate list) solution (puzzleMaps:PuzzleMaps) (alphaset:Set<Candidate>) (candidateLookup:Cell->Set<Candidate>) : (Solution * Hint list) =
    let draw_cell = drawFLFE (List.nth alphabet ((List.length alphabet) / 2))
    let draw_cell2 = drawFL2 (List.nth alphabet ((List.length alphabet) / 2))
    let draw_full (dr:Candidate->'a->ConsoleChar) (symbolTo:Cell->'a) =
        Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL symbolTo puzzleMaps alphabet dr)

    Console.WriteLine item

    if item = "print" then
        draw_full draw_cell solution.grid
        (solution, [])

    else if item.StartsWith "set" then
        let set = ui_set item alphabet solution.grid puzzleMaps
        let newSolution =
            match set with
            | Some setCellValue ->
                let print_grid2 = setCellCandidateGridPre setCellValue puzzleMaps candidateLookup solution.grid
                draw_full draw_cell2 print_grid2

                { solution with
                    grid = setACell setCellValue puzzleMaps candidateLookup solution.grid
                    steps = (SetValue setCellValue) :: solution.steps }
            | None ->
                Console.WriteLine("")
                solution

        print_last newSolution puzzleMaps

        (newSolution, [])

    else if item = "fh" then
        let hints = findFullHouse candidateLookup puzzleMaps

        (solution, List.map FH hints)

    else if item = "hs" then
        let hints = findHiddenSingles alphabet candidateLookup puzzleMaps

        (solution, List.map HS hints)

    else if item = "ns" then
        let hints = findNakedSingles candidateLookup puzzleMaps.cells

        (solution, List.map NS hints)

    else if item = "np" then
        let hints = findNakedPairs candidateLookup puzzleMaps

        (solution, List.map NP hints)

    else
        (solution, [])

let printHint (candidates:Candidate list) (solution:Solution) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) (index:int) (h:Hint) =
    let draw_grid (dr:'a->ConsoleChar) (gridTo:Cell->'a) = Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (gridTo >> dr) puzzleMaps)

    let draw_cell = drawFLFE (List.nth candidates ((List.length candidates) / 2))
    let draw_cell2 = drawFL2 (List.nth candidates ((List.length candidates) / 2))
    let draw_full (dr:Candidate->'a->ConsoleChar) (symbolTo:Cell->'a) =
        Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL symbolTo puzzleMaps candidates dr)

    match h with
    | FH hint ->
        Console.WriteLine ("{0}: {1}", index, fullHouseToString hint)

        let st = fullHouseSymbolTo hint puzzleMaps candidateLookup solution.grid
        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | HS hint ->
        Console.WriteLine ("{0}: {1}", index, formatHiddenSingle hint)

        let st = hiddenSingleSymbolTo hint puzzleMaps candidateLookup solution.grid
        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | NS hint ->
        Console.WriteLine ("{0}: {1}", index, printNakedSingle hint)

        let st = nakedSingleSymbolTo hint candidateLookup solution.grid
        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | NP hint ->
        Console.WriteLine ("{0}: {1}", index, nakedPairToString hint)

        let print_grid2 = nakedPairSymbolTo hint puzzleMaps solution.grid
        draw_full draw_cell2 print_grid2


let run (candidates:Candidate list) (solution:Solution ref) (puzzleMaps:PuzzleMaps) (alphaset:Set<Candidate>) item =
    if item = "quit"
        then
            Some(item)
        else
            let candidateLookup = (!solution).grid >> getCandidateEntries alphaset

            let (soln, hints) = parse item candidates !solution puzzleMaps alphaset candidateLookup

            List.iteri (printHint candidates !solution puzzleMaps candidateLookup) hints

            solution := soln
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

    let line = List.foldBack (puzzleGrid >> symbolOptionToConsoleChar >> drawAnnotatedSymbol >> cons) puzzleMaps.cells [NL]
    List.iter mainWriter line
    mainWriter NL

    let prows = printRowOnOneLine (puzzleGrid >> symbolOptionToConsoleChar >> drawAnnotatedSymbol) puzzleMaps.rowCells sNL puzzleMaps.rows
    Seq.iter mainWriter prows

    Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (puzzleGrid >> symbolOptionToConsoleChar >> drawAnnotatedSymbol) puzzleMaps)

    Seq.tryPick (run candidates solution puzzleMaps candidateSet) readlines |> ignore


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

Maximize() |> ignore

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
