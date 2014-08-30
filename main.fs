module main

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

open console.command
open console.console
open console.format

open core.clearCandidate
open core.puzzlemap
open core.setCell
open core.sudoku
open core.tactics

open hints.fullHouse
open hints.hiddenSingle
open hints.hints
open hints.nakedPair
open hints.nakedSingle

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

type Hint = 
    | FH of FullHouse
    | HS of HiddenSingle
    | NS of NakedSingle
    | NP of NakedPair

let symbolToEntry (puzzleMaps : PuzzleMaps) (symbolLookup : Cell -> Symbol option) = 
    fun (cell : Cell) -> 
        match symbolLookup cell with
        | Some(e) -> Given(e)
        | None -> 
            let symbols = houseCellsForCell puzzleMaps cell symbolLookup
            let candidates = Set.map symbolToCandidate symbols
            Candidates(fun candidate -> 
                if Set.contains candidate candidates then Excluded
                else Possible)

type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write : ConsoleCharWriter) (grid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (action : Action) 
    (puzzleMaps : PuzzleMaps) = 
    Seq.iter write (printGrid defaultGridChars sNL (grid >> drawAnnotatedSymbol) puzzleMaps)
    match action with
    | SetCellValue sv -> write (CStr(sv.ToString()))
    | ClearCandidate cc -> write (CStr(cc.ToString()))
    write NL

let print_last (steps : Action list) (grid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (puzzleMaps : PuzzleMaps) = 
    match steps with
    | s :: _ -> print_step ConsoleWriteChar grid s puzzleMaps
    | [] -> ()

let parse (item : string) (alphabet : Candidate list) solution (puzzleMaps : PuzzleMaps) 
    (candidateLookup : Cell -> Set<Candidate>) : Solution * Hint list = 
    let draw_cell = drawFLFE (List.nth alphabet ((List.length alphabet) / 2))
    let draw_cell2 = drawFL2 (List.nth alphabet ((List.length alphabet) / 2))
    let draw_full (dr : Candidate -> 'a -> ConsoleChar) (symbolTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL symbolTo puzzleMaps alphabet dr)
    Console.WriteLine item
    if item = "print" then 
        draw_full draw_cell solution.grid
        (solution, [])
    else if item.StartsWith "set" then 
        let commandset = ui_set item alphabet solution.grid puzzleMaps
        
        let newSolution = 
            match commandset with
            | Some setCellValue -> 
                let hd = 
                    { HintDescription.house = None
                      candidateReductions = set []
                      setCellValue = Some setCellValue
                      pointerCells = set []
                      pointerCandidates = set [] }
                
                let print_grid2 = mhas hd puzzleMaps candidateLookup solution.grid
                draw_full draw_cell2 print_grid2
                { solution with grid = setCellApply setCellValue puzzleMaps candidateLookup solution.grid
                                steps = (SetCellValue setCellValue) :: solution.steps }
            | None -> 
                Console.WriteLine("")
                solution
        print_last newSolution.steps newSolution.grid puzzleMaps
        (newSolution, [])
    else if item = "fh" then 
        let hints = fullHouseFind candidateLookup puzzleMaps
        (solution, List.map FH hints)
    else if item = "hs" then 
        let hints = hiddenSingleFind alphabet candidateLookup puzzleMaps
        (solution, List.map HS hints)
    else if item = "ns" then 
        let hints = nakedSingleFind candidateLookup puzzleMaps.cells

        (solution, List.map NS hints)

    else if item = "np" then 
        let hints = nakedPairFind candidateLookup puzzleMaps

        (solution, List.map NP hints)

    else (solution, [])

let printHint (candidates : Candidate list) (solution : Solution) (puzzleMaps : PuzzleMaps) 
    (candidateLookup : Cell -> Set<Candidate>) (index : int) (h : Hint) = 
    let draw_grid (dr : 'a -> ConsoleChar) (gridTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (gridTo >> dr) puzzleMaps)

    let draw_cell2 = drawFL2 (List.nth candidates ((List.length candidates) / 2))
    let draw_full (dr : Candidate -> 'a -> ConsoleChar) (symbolTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (print_full defaultSolutionChars sNL symbolTo puzzleMaps candidates dr)

    match h with
    | FH hint -> 
        Console.WriteLine("{0}: {1}", index, hint)

        let hd = fullHouseToDescription hint puzzleMaps candidateLookup

        let st = mhas hd puzzleMaps candidateLookup solution.grid

        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | HS hint -> 
        Console.WriteLine("{0}: {1}", index, hint)

        let hd = hiddenSingleToDescription hint puzzleMaps candidateLookup

        let st = mhas hd puzzleMaps candidateLookup solution.grid

        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | NS hint -> 
        Console.WriteLine("{0}: {1}", index, hint)

        let hd = nakedSingleToDescription hint puzzleMaps candidateLookup

        let st = mhas hd puzzleMaps candidateLookup solution.grid

        draw_grid drawHintAnnotatedSymbol st

        draw_full draw_cell2 st

    | NP hint -> 
        Console.WriteLine("{0}: {1}", index, hint)

        let hd = nakedPairToDescription hint puzzleMaps

        let print_grid2 = mhas hd puzzleMaps candidateLookup solution.grid

        draw_full draw_cell2 print_grid2

    Console.Read() |> ignore

let run (candidates : Candidate list) (solution : Solution ref) (puzzleMaps : PuzzleMaps) item = 
    if item = "quit" then Some "quit"
    else 
        let alphaset = Set.ofList candidates
        
        let getCandidateEntries = 
            function 
            | Candidates s -> Set.filter (fun candidate -> s candidate = Possible) alphaset
            | _ -> Set.empty
        
        let candidateLookup = (!solution).grid >> getCandidateEntries

        let (soln, hints) = parse item candidates !solution puzzleMaps candidateLookup

        solution := soln

        List.iteri (printHint candidates !solution puzzleMaps candidateLookup) hints

        None


let mainWriter = ConsoleWriteChar

let flattenEntry (cellLookup : Cell -> 'a) (cells : Cell list) = 
    let s = List.map (fun cell -> (cell, cellLookup cell)) cells

    let s2 = s |> Map.ofList

    let solutionGrid = new System.Collections.Generic.Dictionary<Cell, 'a>(s2)

    fun cell -> solutionGrid.[cell]

let repl (sudoku : string) (puzzleSpec : Puzzle) = 

    Console.WriteLine sudoku

    let alphabetisedLine = loadLine sudoku puzzleSpec.alphabet

    let puzzleMaps = makePuzzleMaps puzzleSpec

    let puzzleGrid = loadPuzzle alphabetisedLine puzzleMaps.cells

    let candidates = List.map symbolToCandidate puzzleSpec.alphabet

    let stoe = symbolToEntry puzzleMaps puzzleGrid

    let solutionGrid = flattenEntry stoe puzzleMaps.cells
    
    let line = 
        List.foldBack (puzzleGrid
                       >> symbolOptionToConsoleChar
                       >> drawAnnotatedSymbol
                       >> cons) puzzleMaps.cells [ NL ]
    List.iter mainWriter line
    mainWriter NL

    let prows = 
        printRowOnOneLine (puzzleGrid
                           >> symbolOptionToConsoleChar
                           >> drawAnnotatedSymbol) puzzleMaps.rowCells sNL puzzleMaps.rows
    Seq.iter mainWriter prows

    Seq.iter ConsoleWriteChar (printGrid defaultGridChars sNL (puzzleGrid
                      >> symbolOptionToConsoleChar
                      >> drawAnnotatedSymbol) puzzleMaps)

    let solution = 
        ref ({ grid = solutionGrid
               steps = [] })
    
    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))

    Seq.tryPick (run candidates solution puzzleMaps) readlines |> ignore

let defaultPuzzleSpec = 
    { boxWidth = 3 * 1<width>
      boxHeight = 3 * 1<height>
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Symbol ]
      symbols = fun _ -> None }

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
