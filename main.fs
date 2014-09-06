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

open hints.boxLineReduction
open hints.fullHouse
open hints.hiddenPair
open hints.hiddenQuad
open hints.hiddenSingle
open hints.hiddenTriple
open hints.hints
open hints.nakedPair
open hints.nakedQuad
open hints.nakedSingle
open hints.nakedTriple
open hints.pointingPair

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

type Hint = 
    | FH of HintDescription
    | HS of HintDescription
    | HP of HintDescription
    | HT of HintDescription
    | HQ of HintDescription
    | NS of HintDescription
    | NP of HintDescription
    | NT of HintDescription
    | NQ of HintDescription
    | PP of HintDescription
    | BL of HintDescription

let symbolToEntry (puzzleSpec : Puzzle) (symbolLookup : Cell -> Symbol option) = 
    let puzzleHouseCellCells = houseCellCells puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight
    
    let houseCellsForCell (cell : Cell) (symbolLookup : Cell -> 'a option) = 
        let a = puzzleHouseCellCells cell

        let sys = Set.map symbolLookup a
        let sys2 = Set.filter (fun (s : 'a option) -> s.IsSome) sys
        Set.map (fun (s : 'a option) -> s.Value) sys2

    fun (cell : Cell) -> 
        match symbolLookup cell with
        | Some(e) -> Given(e)
        | None -> 
            let symbols = houseCellsForCell cell symbolLookup
            let candidates = Set.map symbolToCandidate symbols
            Candidates(fun candidate -> 
                if Set.contains candidate candidates then Excluded
                else Possible)

type ConsoleCharWriter = ConsoleChar -> Unit

let print_step (write : ConsoleCharWriter) (grid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (action : Action) 
    (puzzleSpec : Puzzle) = 
    let puzzlePrintGrid = printGrid puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight

    Seq.iter write (puzzlePrintGrid defaultGridChars sNL (grid >> drawAnnotatedSymbol))
    match action with
    | SetCellValue sv -> write (CStr(sv.ToString()))
    | ClearCandidate cc -> write (CStr(cc.ToString()))
    write NL

let print_last (steps : Action list) (grid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (puzzleSpec : Puzzle) = 
    match steps with
    | s :: _ -> print_step ConsoleWriteChar grid s puzzleSpec
    | [] -> ()

let parse (item : string) (alphabet : Candidate list) solution (puzzleSpec : Puzzle) 
    (candidateLookup : Cell -> Set<Candidate>) : Solution * Hint list = 
    let puzzlePrintFull (solutionChars : solutionChars<seq<'c>>) (eol : seq<'c>) (symbolTo : Cell -> 'b) 
        (alphabet : Candidate list) (drawCell : Candidate -> 'b -> 'c) = 
        print_full puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight solutionChars eol symbolTo alphabet drawCell
    let puzzleHouseCells = houseCells puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight
    let puzzleHouseCellCells = houseCellCells puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight
    let puzzleCells = cells puzzleSpec.size
    let puzzleHouses = houses puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight

    let draw_cell = drawFLFE (List.nth alphabet ((List.length alphabet) / 2))
    let draw_cell2 = drawFL2 (List.nth alphabet ((List.length alphabet) / 2))
    let draw_full (dr : Candidate -> 'a -> ConsoleChar) (symbolTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (puzzlePrintFull defaultSolutionChars sNL symbolTo alphabet dr)

    Console.WriteLine item

    if item = "print" then 
        draw_full draw_cell solution.grid
        (solution, [])
    else if item.StartsWith "set" then 
        let commandset = ui_set item alphabet solution.grid puzzleCells
        
        let newSolution = 
            match commandset with
            | Some setCellValue -> 
                let hd = 
                    { HintDescription.house = None
                      candidateReductions = set []
                      setCellValue = Some setCellValue
                      pointers = set [] }
                
                let print_grid2 = mhas hd puzzleHouseCells puzzleHouseCellCells candidateLookup solution.grid
                draw_full draw_cell2 print_grid2
                { solution with grid = setCellApply setCellValue puzzleHouseCellCells candidateLookup solution.grid
                                steps = (SetCellValue setCellValue) :: solution.steps }
            | None -> 
                Console.WriteLine("")
                solution
        print_last newSolution.steps newSolution.grid puzzleSpec
        (newSolution, [])
    else if item = "fh" then 
        let hints = fullHouseFind candidateLookup puzzleHouseCells puzzleHouses
        (solution, List.map FH hints)
    else if item = "hs" then 
        let hints = hiddenSingleFind alphabet candidateLookup puzzleHouseCells puzzleHouses
        (solution, List.map HS hints)
    else if item = "hp" then 
        let hints = hiddenPairFind alphabet candidateLookup puzzleHouseCells puzzleHouses
        (solution, List.map HP hints)
    else if item = "ht" then 
        let hints = hiddenTripleFind alphabet candidateLookup puzzleHouseCells puzzleHouses
        (solution, List.map HT hints)
    else if item = "hq" then 
        let hints = hiddenQuadFind alphabet candidateLookup puzzleHouseCells puzzleHouses
        (solution, List.map HQ hints)
    else if item = "ns" then 
        let hints = nakedSingleFind candidateLookup puzzleCells

        (solution, List.map NS hints)

    else if item = "np" then 
        let hints = nakedPairFind candidateLookup puzzleHouseCells puzzleHouses

        (solution, List.map NP hints)

    else if item = "nt" then 
        let hints = nakedTripleFind candidateLookup puzzleHouseCells puzzleHouses

        (solution, List.map NT hints)

    else if item = "nq" then 
        let hints = nakedQuadFind candidateLookup puzzleHouseCells puzzleHouses

        (solution, List.map NQ hints)

    else if item = "pp" then 
        let hints = pointingPairFind candidateLookup puzzleHouseCells puzzleHouses

        (solution, List.map PP hints)

    else if item = "bl" then 
        let hints = 
            boxLineReductionFind candidateLookup puzzleHouseCells puzzleHouses puzzleSpec.boxWidth puzzleSpec.boxHeight
        (solution, List.map BL hints)

    else (solution, [])

let printHint (candidates : Candidate list) (solution : Solution) (puzzleSpec : Puzzle) 
    (candidateLookup : Cell -> Set<Candidate>) (index : int) (h : Hint) = 
    let puzzlePrintFull (solutionChars : solutionChars<seq<'c>>) (eol : seq<'c>) (symbolTo : Cell -> 'b) 
        (alphabet : Candidate list) (drawCell : Candidate -> 'b -> 'c) = 
        print_full puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight solutionChars eol symbolTo alphabet drawCell
    let puzzleHouseCells = houseCells puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight
    let puzzleHouseCellCells = houseCellCells puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight
    let puzzlePrintGrid = printGrid puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight

    let draw_grid (dr : 'a -> ConsoleChar) (gridTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (puzzlePrintGrid defaultGridChars sNL (gridTo >> dr))

    let draw_cell2 = drawFL2 (List.nth candidates ((List.length candidates) / 2))
    let draw_full (dr : Candidate -> 'a -> ConsoleChar) (symbolTo : Cell -> 'a) = 
        Seq.iter ConsoleWriteChar (puzzlePrintFull defaultSolutionChars sNL symbolTo candidates dr)
    
    let draw_full_hint index hint = 
        Console.WriteLine("{0}: {1}", index, hint)

        let print_grid2 = mhas hint puzzleHouseCells puzzleHouseCellCells candidateLookup solution.grid
        draw_full draw_cell2 print_grid2

    match h with
    | FH hint -> draw_full_hint index hint
    | HS hint -> draw_full_hint index hint
    | HP hint -> draw_full_hint index hint
    | HT hint -> draw_full_hint index hint
    | HQ hint -> draw_full_hint index hint
    | NS hint -> draw_full_hint index hint
    | NP hint -> draw_full_hint index hint
    | NT hint -> draw_full_hint index hint
    | NQ hint -> draw_full_hint index hint
    | PP hint -> draw_full_hint index hint
    | BL hint -> draw_full_hint index hint
    Console.Read() |> ignore

let run (candidates : Candidate list) (solution : Solution ref) (puzzleSpec : Puzzle) item = 
    if item = "quit" then Some "quit"
    else 
        let alphaset = Set.ofList candidates
        
        let getCandidateEntries = 
            function 
            | Candidates s -> Set.filter (fun candidate -> s candidate = Possible) alphaset
            | _ -> Set.empty
        
        let candidateLookup = (!solution).grid >> getCandidateEntries

        let (soln, hints) = parse item candidates !solution puzzleSpec candidateLookup

        solution := soln

        List.iteri (printHint candidates !solution puzzleSpec candidateLookup) hints

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

    let puzzleGrid = loadPuzzle alphabetisedLine (cells puzzleSpec.size)

    let candidates = List.map symbolToCandidate puzzleSpec.alphabet

    let stoe = symbolToEntry puzzleSpec puzzleGrid

    let solutionGrid = flattenEntry stoe (cells puzzleSpec.size)

    let cons x y = x :: y
    
    let line = 
        List.foldBack (puzzleGrid
                       >> symbolOptionToConsoleChar
                       >> drawAnnotatedSymbol
                       >> cons) (cells puzzleSpec.size) [ NL ]
    List.iter mainWriter line
    mainWriter NL

    let prows = 
        printRowOnOneLine (puzzleGrid
                           >> symbolOptionToConsoleChar
                           >> drawAnnotatedSymbol) (rowCells puzzleSpec.size) sNL (rows puzzleSpec.size)
    Seq.iter mainWriter prows

    Seq.iter ConsoleWriteChar 
        (printGrid puzzleSpec.size puzzleSpec.boxWidth puzzleSpec.boxHeight defaultGridChars sNL 
             (puzzleGrid
              >> symbolOptionToConsoleChar
              >> drawAnnotatedSymbol))

    let solution = 
        ref ({ grid = solutionGrid
               steps = [] })
    
    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))

    Seq.tryPick (run candidates solution puzzleSpec) readlines |> ignore

let defaultPuzzleSpec = 
    { boxWidth = 3 * 1<width>
      boxHeight = 3 * 1<height>
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Symbol ]
      size = 9 * 1<size>
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
//let example = "801006094300009080970080500547062030632000050198375246083620915065198000219500008"
//let example = "2...3..7.9...1..8.5...6.9.4653871492489325761721496.....5.8.....6..4.....9..5...3"

// ht
//let example = "528600049136490025794205630000100200007826300002509060240300976809702413070904582"
// hq
//let example = "...3742......82.4..............3.8266...9...48.5.4697.547.2...9......4.5.1.45.7.2"
// http://www.sudokuwiki.org/Hidden_Candidates hq
//let example = "65..87.24...649.5..4..25...57.438.61...5.1...31.9.2.85...89..1....213...13.75..98"
//let example = "000500000425090001800010020500000000019000460000000002090040003200060807000001600"

repl example defaultPuzzleSpec

Console.WriteLine "bye"
