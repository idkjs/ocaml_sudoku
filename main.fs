module main

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

open core.sudoku
open core.puzzlemap
open core.hints
open core.setCell
open core.eliminateCandidate
open core.force

open hints.fullHouse
open hints.hidden
open hints.intersection
open hints.naked
open hints.wing

open console.command
open console.console
open console.format

open load

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

type HintType = 
    | FH
    | HS
    | HP
    | HT
    | HQ
    | NS
    | NP
    | NT
    | NQ
    | PP
    | BL
    | X
    | Y

let SupportedHints : Map<string, HintType * (PuzzleMap -> CellCandidates -> Set<HintDescription2>)> =
    [
        ("fh", (FH, fullHouses))
        ("hs", (HS, hiddenN 1))
        ("hp", (HP, hiddenN 2))
        ("ht", (HT, hiddenN 3))
        ("hq", (HQ, hiddenN 4))
        ("ns", (NS, nakedSingle))
        ("np", (NP, nakedN 2))
        ("nt", (NT, nakedN 3))
        ("nq", (NQ, nakedN 4))
        ("pp", (PP, pointingPairs))
        ("bl", (BL, boxLineReductions))
        ("x",  (X,  xWings))
        ("y",  (Y,  yWings))
    ]
    |> Map.ofList

type Hint = HintType * HintDescription2

let parse (item : string) (solution : Solution) (puzzle : PuzzleShape) 
    (candidateLookup : CellCandidates) puzzleDrawFull puzzleDrawFull2 print_last : Solution * HintType option * Set<HintDescription2> = 

    let p = TPuzzleMap puzzle :> PuzzleMap

    Console.WriteLine item

    if item = "print" then 
        puzzleDrawFull None
        (solution, None, Set.empty)
    else if item.StartsWith "focus" then
        let terms = item.Split(' ')
        let focusDigit =
            if terms.Length = 2 then 
                parseValue puzzle.alphabet terms.[1]
            else
                None
        puzzleDrawFull focusDigit
        (solution, None, Set.empty)
    else if item.StartsWith "s" then 
        let setCommand = setCellCommand item puzzle.alphabet solution.current p.cells p.cellHouseCells candidateLookup
        
        let newSolution = 
            match setCommand with
            | Some setCellValue -> 
                let hd2 = setCellHintDescription p setCellValue
                puzzleDrawFull2 hd2.annotations

                setCellStep p setCellValue solution
            | None -> 
                Console.WriteLine("")
                solution
        print_last newSolution
        (newSolution, None, Set.empty)

    else if item.StartsWith "c" then 
        let clearCommand = candidateClearCommand item puzzle.alphabet solution.current p.cells
        
        let newSolution = 
            match clearCommand with
            | Some candidate -> 
                let cr = 
                    { CandidateReduction.cell = candidate.cell
                      candidates = set [ candidate.digit ] }
                
                let hd = 
                    { HintDescription.primaryHouses = set []
                      secondaryHouses = set []
                      candidateReductions = set [ cr ]
                      setCellValueAction = None
                      pointers = set [] }
                
                let hd2 = mhas p.cells p.cellHouseCells p.houseCells hd
                puzzleDrawFull2 hd2.annotations
                { solution with current = eliminateCandidateApply candidate solution.current
                                steps = (Eliminate candidate) :: solution.steps }
            | None -> 
                Console.WriteLine("")
                solution

        print_last newSolution
        (newSolution, None, Set.empty)

    else
        let supportedHintOpt = SupportedHints.TryFind item
        match supportedHintOpt with
        | Some supportedHint ->
            let hints = (snd supportedHint) p candidateLookup
            (solution, Some (fst supportedHint), hints)
        | None -> (solution, None, Set.empty)

let printHint drawHint (index : int) (hint : HintDescription2) : unit = 

    Console.WriteLine("{0}: {1}", index, hint)

    drawHint hint.annotations

    Console.Read() |> ignore

let run (solution : Solution ref) (puzzle : PuzzleShape) 
    puzzleDrawFull puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint item = 
    if item = "quit" then Some "quit"
    else 
        let getCandidateEntries (_ : Cell) (annotatedDigit : CellContents) : Set<Digit> = 
            match annotatedDigit with
            | BigNumber _ -> Set.empty
            | PencilMarks s -> s

        let candidateLookup =
            (!solution).current
            |> Map.map getCandidateEntries

        let mapCandidateCandidates = MapCellCandidates candidateLookup :> CellCandidates

        let (soln, hintTypeOpt, hints) = 
            parse item !solution puzzle mapCandidateCandidates puzzleDrawFull 
                puzzleDrawCandidateGridAnnotations print_last
        solution := soln

        Seq.iteri puzzlePrintHint hints

        None

let digitToEntry (cellDigitLookup : Given) (alphabet : Set<Digit>) 
    (cellHouseCells : CellHouseCells) : Current = 
    let m (cell : Cell) (dop : Digit option) : CellContents =
        match dop with
        | Some(e) -> BigNumber(e)
        | None -> 
            let cells =
                cell
                |> cellHouseCells.Get
                |> Set.toList
            let digits =
                cells
                |> List.choose (fun cell -> cellDigitLookup.Item cell)
                |> Set.ofList

            Set.difference alphabet digits
            |> PencilMarks
    cellDigitLookup
    |> Map.map m

let repl (sudoku : string) (puzzle : PuzzleShape) = 

    Console.WriteLine sudoku
    
    let p = TPuzzleMap puzzle :> PuzzleMap

    let transformer (puzzleGrid : Given) : Current = 
        digitToEntry puzzleGrid (Set.ofList puzzle.alphabet) p.cellHouseCells
    
    let solution = ref (load p.orderedCells puzzle.alphabet (List.ofSeq sudoku) transformer)

    let centreDigit : Digit = List.nth puzzle.alphabet ((List.length puzzle.alphabet) / 2)

    let puzzlePrintLine = printLine p.orderedCells

    let puzzlePrintGrid = printGrid p.orderedStacks p.orderedStackColumns p.orderedBands p.orderedBandRows defaultGridChars

    let puzzlePrintCandidateGrid = 
        printCandidateGrid p.orderedStacks p.orderedStackColumns p.orderedBands p.orderedBandRows defaultCandidateGridChars 
            puzzle.alphabet

    // Print a Digit option, with colours
    let puzzleDrawCell (solution : Solution) (cell : Cell) : ConsoleChar = 
        drawDigitCellContents (solution.given.Item cell) (solution.current.Item cell)

    let puzzleDrawCellDigit (focus : Digit option) (solution : Solution) (cell : Cell) (digit : Digit) : ConsoleChar = 
        drawDigitCellContentAnnotations centreDigit focus digit (solution.given.Item cell) 
            (solution.current.Item cell) None

    let puzzleDrawCellDigitAnnotations (solution : Solution) (l : Map<Cell, CellAnnotation>) (cell : Cell) (digit : Digit) : ConsoleChar = 
        drawDigitCellContentAnnotations centreDigit None digit (solution.given.Item cell) 
            (solution.current.Item cell) (Some(l.Item cell))

    let puzzleDrawLine () =
        Seq.iter drawConsoleChar (puzzlePrintLine (puzzleDrawCell !solution))

    let puzzleDrawGrid () =
        Seq.iter drawConsoleChar (puzzlePrintGrid (puzzleDrawCell !solution))
    
    let puzzleDrawCandidateGrid (focus : Digit option) : unit = 
        Seq.iter drawConsoleChar (puzzlePrintCandidateGrid (puzzleDrawCellDigit focus !solution))
    
    let puzzleDrawCandidateGridAnnotations annotations = 
        Seq.iter drawConsoleChar (puzzlePrintCandidateGrid (puzzleDrawCellDigitAnnotations !solution annotations))

    let print_last (solution : Solution) = 
        let puzzleDrawCell' (cell : Cell) = drawDigitCellContents (solution.given.Item cell) (solution.current.Item cell)
        Seq.iter drawConsoleChar (puzzlePrintGrid puzzleDrawCell')

        match solution.steps with
        | action :: _ -> 
            match action with
            | Placement sv -> drawConsoleChar (CStr(sv.ToString()))
            | Eliminate candidate -> drawConsoleChar (CStr(candidate.ToString()))
        | [] -> ()

        drawConsoleChar NL

    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))

    puzzleDrawLine()
    drawConsoleChar NL

    puzzleDrawGrid()

    //let forcedSolutions = solve (!solution) p.cells p.cellHouseCells
    //puzzleDrawGrid()
    //if List.length forcedSolutions > 0 then
    //    List.iter
    //        (fun solve -> Seq.iter drawConsoleChar (puzzlePrintGrid (puzzleDrawCell solve)))
    //        forcedSolutions
    //else Console.WriteLine("No solutions")

    let puzzlePrintHint = printHint puzzleDrawCandidateGridAnnotations

    Seq.tryPick (run solution puzzle puzzleDrawCandidateGrid puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint) 
        readlines |> ignore

let defaultPuzzleSpec = 
    { size = 9 * 1<size>
      boxWidth = 3 * 1<boxWidth>
      boxHeight = 3 * 1<boxHeight>
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Digit ] }

(*
let defaultPuzzleSpec = {
    boxWidth = 4 * 1<width>
    boxHeight = 2 * 1<height>
    alphabet = [ for i in 1 .. 8 -> (char) i + '0' |> Digit ]
    digits = fun _ -> None
}
*)

Maximize() |> ignore

// Input puzzle
Console.WriteLine "1........2........3........4........5........6........7........8........9........"
Console.WriteLine "123456789123456789123456789123456789123456789123456789123456789123456789123456789"

//let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800"
let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000"

// FullHouse
//let example = "800739006370465000040182009000600040054300610060500000400853070000271064100940002"
//let example = "801006094300009080970080500547062030632000050198375246083620915065198000219500008"
//let example = "2...3..7.9...1..8.5...6.9.4653871492489325761721496.....5.8.....6..4.....9..5...3"

// ht
//let example = "528600049136490025794205630000100200007826300002509060240300976809702413070904582"
// hq
//let example = "...3742......82.4..............3.8266...9...48.5.4697.547.2...9......4.5.1.45.7.2"
// http://www.sudokuwiki.org/Hidden_Candidates hq
//let example = "65..87.24...649.5..4..25...57.438.61...5.1...31.9.2.85...89..1....213...13.75..98"
//let example = "000500000425090001800010020500000000019000460000000002090040003200060807000001600"

// http://www.sudokuwiki.org/X_Wing_Strategy
//let example = "100000569492056108056109240009640801064010000218035604040500016905061402621000005"
// http://www.sudokuwiki.org/Y_Wing_Strategy
//let example = "900240000050690231020050090090700320002935607070002900069020073510079062207086009"
//let example = "273005081810302004009010200100953728792186345538724196021060500300201869080530412"


repl example defaultPuzzleSpec

Console.WriteLine "bye"
