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

open hints.fullHouse
open hints.hidden
open hints.hints
open hints.intersection
open hints.naked
open hints.wing

open load

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

[<NoEquality; NoComparison>]
type Hint = 
    | FH of HintDescription2
    | HS of HintDescription2
    | HP of HintDescription2
    | HT of HintDescription2
    | HQ of HintDescription2
    | NS of HintDescription2
    | NP of HintDescription2
    | NT of HintDescription2
    | NQ of HintDescription2
    | PP of HintDescription2
    | BL of HintDescription2
    | X of HintDescription2
    | Y of HintDescription2

let print_last (solution : Solution) puzzlePrintGrid = 

    let drawer (cell : Cell) = drawSymbolCellContents (solution.start cell) (solution.current cell)
    Seq.iter drawConsoleChar (puzzlePrintGrid drawer)

    match solution.steps with
    | action :: _ ->
        match action with
        | SetCellSymbol sv -> drawConsoleChar(CStr(sv.ToString()))
        | ClearCellCandidate cc -> drawConsoleChar(CStr(cc.ToString()))

    | [] -> ()

    drawConsoleChar NL

let parse (item : string) (alphabet : Candidate list) (solution : Solution) (puzzle : Puzzle) 
    (candidateLookup : Cell -> Set<Candidate>) puzzlePrintGrid puzzlePrintFull puzzleDrawFull puzzleDrawFull2 : Solution * Hint list = 

    let puzzleSize = (List.length alphabet) * 1<size>
    let puzzleHouseCells = houseCells puzzleSize puzzle.boxWidth puzzle.boxHeight
    let puzzleHouseCellCells = houseCellCells puzzleSize puzzle.boxWidth puzzle.boxHeight
    let puzzleCells = cells puzzleSize
    let puzzleHouses = houses puzzleSize puzzle.boxWidth puzzle.boxHeight
    let puzzleBoxes = boxes puzzleSize puzzle.boxWidth puzzle.boxHeight
    let puzzleRows = rows puzzleSize
    let puzzleCols = columns puzzleSize
    let puzzleCellBox = cellBox puzzle.boxWidth puzzle.boxHeight



    Console.WriteLine item

    if item = "print" then 
        puzzleDrawFull()
        (solution, [])
    else if item.StartsWith "s" then 
        let setCommand = setCellCommand item alphabet solution.current puzzleCells puzzleHouseCellCells candidateLookup
        
        let newSolution = 
            match setCommand with
            | Some setCellValue -> 
                let hd = 
                    { HintDescription.primaryHouses = set []
                      secondaryHouses = set []
                      candidateReductions = set []
                      setCellValue = Some setCellValue
                      pointers = set [] }
                let hd2 = mhas puzzleHouseCellCells puzzleHouseCells hd
                puzzleDrawFull2 hd2.annotations
                { solution with current = 
                                    setCellSymbolApply puzzleHouseCellCells setCellValue solution.current
                                steps = (SetCellSymbol setCellValue) :: solution.steps }
            | None -> 
                Console.WriteLine("")
                solution
        print_last newSolution puzzlePrintGrid
        (newSolution, [])

    else if item.StartsWith "c" then 
        let clearCommand = candidateClearCommand item alphabet solution.current puzzleCells
        
        let newSolution = 
            match clearCommand with
            | Some clearCandidate -> 
                let cr = 
                    { CandidateReduction.cell = clearCandidate.cell
                      candidates = set [ clearCandidate.candidate ] }
                
                let hd = 
                    { HintDescription.primaryHouses = set []
                      secondaryHouses = set []
                      candidateReductions = set [ cr ]
                      setCellValue = None
                      pointers = set [] }
                let hd2 = mhas puzzleHouseCellCells puzzleHouseCells hd
                puzzleDrawFull2 hd2.annotations
                { solution with current = clearCandidateApply clearCandidate solution.current
                                steps = (ClearCellCandidate clearCandidate) :: solution.steps }
            | None -> 
                Console.WriteLine("")
                solution

        print_last newSolution puzzlePrintGrid
        (newSolution, [])

    else if item = "fh" then 
        let hints = List.collect (fullHousePerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup) puzzleHouses
        (solution, List.map FH hints)
    else if item = "hs" then 
        let hints = List.collect (hiddenNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 1) puzzleHouses
        (solution, List.map HS hints)
    else if item = "hp" then 
        let hints = List.collect (hiddenNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 2) puzzleHouses
        (solution, List.map HP hints)
    else if item = "ht" then 
        let hints = List.collect (hiddenNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 3) puzzleHouses
        (solution, List.map HT hints)
    else if item = "hq" then 
        let hints = List.collect (hiddenNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 4) puzzleHouses
        (solution, List.map HQ hints)
    else if item = "ns" then 
        let hints = nakedSingleFind puzzleHouseCellCells puzzleHouseCells candidateLookup puzzleCells

        (solution, List.map NS hints)

    else if item = "np" then 
        let hints = List.collect (nakedNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 2) puzzleHouses

        (solution, List.map NP hints)

    else if item = "nt" then 
        let hints = List.collect (nakedNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 3) puzzleHouses

        (solution, List.map NT hints)

    else if item = "nq" then 
        let hints = List.collect (nakedNPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup 4) puzzleHouses

        (solution, List.map NQ hints)

    else if item = "pp" then 
        let hints = List.collect (pointingPairsPerBox puzzleHouseCellCells puzzleHouseCells candidateLookup) (List.map Box puzzleBoxes)

        (solution, List.map PP hints)

    else if item = "bl" then 
        let rowHints = 
            List.collect 
                (boxLineReductionsPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup puzzleCellBox) 
                (List.map Row puzzleRows)
        let colHints = 
            List.collect 
                (boxLineReductionsPerHouse puzzleHouseCellCells puzzleHouseCells candidateLookup puzzleCellBox) 
                (List.map Column puzzleCols)
        let hints = List.concat [ rowHints; colHints ]

        (solution, List.map BL hints)

    else if item = "x" then 
        let hints = xWingFind puzzleHouseCellCells puzzleHouseCells candidateLookup puzzleRows puzzleCols

        (solution, List.map X hints)

    else if item = "y" then 
        let hints = yWingFind puzzleHouseCellCells puzzleHouseCells candidateLookup puzzleRows puzzleCols

        (solution, List.map Y hints)

    else (solution, [])

let printHint puzzleHouseCells puzzleHouseCellCells drawHint (index : int) (h : Hint) = 

    let draw_full_hint (index : int) (hint : HintDescription2) = 
        Console.WriteLine("{0}: {1}", index, hint)

        drawHint hint.annotations

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
    | X hint -> draw_full_hint index hint
    | Y hint -> draw_full_hint index hint

    Console.Read() |> ignore

let run (alphabet : Candidate list) (solution : Solution ref) (puzzle : Puzzle) puzzlePrintGrid puzzlePrintFull puzzleDrawFull puzzleDrawFullHint item = 
    if item = "quit" then Some "quit"
    else 
        let alphaset = Set.ofList alphabet

        let puzzleSize = (List.length alphabet) * 1<size>
        let puzzleHouseCells = houseCells puzzleSize puzzle.boxWidth puzzle.boxHeight
        let puzzleHouseCellCells = houseCellCells puzzleSize puzzle.boxWidth puzzle.boxHeight
        
        let getCandidateEntries (annotatedSymbol : CellContents) = 
            match annotatedSymbol with
            | ASymbol _ -> Set.empty
            | ACandidates s -> s
        
        let candidateLookup = (!solution).current >> getCandidateEntries

        let (soln, hints) = parse item alphabet !solution puzzle candidateLookup puzzlePrintGrid puzzlePrintFull puzzleDrawFull puzzleDrawFullHint

        solution := soln



        List.iteri (printHint puzzleHouseCells puzzleHouseCellCells puzzleDrawFullHint) hints

        None


let mainWriter = drawConsoleChar

let memoiseCellLookup (cells : Cell list) (cellLookup : Cell -> 'a) : (Cell -> 'a) = 
    let s = List.map (fun cell -> (cell, cellLookup cell)) cells

    let s2 = s |> Map.ofList

    let solutionGrid = new System.Collections.Generic.Dictionary<Cell, 'a>(s2)

    fun cell -> solutionGrid.[cell]


let symbolToEntry (cellSymbolLookup : Cell -> Symbol option) (alphabet : Set<Symbol>) (houseCellCells : Cell -> Set<Cell>) : (Cell -> CellContents) = 
    fun (cell : Cell) -> 
        match cellSymbolLookup cell with
        | Some(e) -> ASymbol(e)
        | None -> 
            let cells = houseCellCells cell |> Set.toList
            let symbols = List.choose cellSymbolLookup cells |> Set.ofList
            let possibleSymbols = Set.difference alphabet symbols
            let possibleCandidates = Set.map symbolToCandidate possibleSymbols
            ACandidates possibleCandidates

let repl (sudoku : string) (puzzle : Puzzle) = 

    Console.WriteLine sudoku

    let puzzleSize = (List.length puzzle.alphabet) * 1<size>
    let puzzleCells = cells puzzleSize
    let puzzleRowCells = rowCells puzzleSize
    let puzzleStacks = stacks puzzleSize puzzle.boxWidth
    let puzzleStackColumns = stackColumns puzzle.boxWidth
    let puzzleBands = bands puzzleSize puzzle.boxHeight
    let puzzleBandRows = bandRows puzzle.boxHeight
    let puzzleHouseCellCells = houseCellCells puzzleSize puzzle.boxWidth puzzle.boxHeight

    let transformer (puzzleGrid : Cell -> Symbol option) : (Cell -> CellContents) =
        let stoe = symbolToEntry puzzleGrid (Set.ofList puzzle.alphabet) puzzleHouseCellCells

        memoiseCellLookup puzzleCells stoe

    let solution = ref (load puzzle.alphabet (List.ofSeq sudoku) transformer)

    let candidates = List.map symbolToCandidate puzzle.alphabet

    let puzzlePrintFull = 
        printCellAndCandidates puzzleStacks puzzleStackColumns puzzleBands puzzleBandRows defaultSolutionChars candidates

    let cons x y = x :: y
    
    // Print a symbol option, with colours
    let symbolOptionToConsoleChar (cell : Cell) : ConsoleChar = 
        drawSymbolCellContents ((!solution).start cell) ((!solution).current cell)

    let line = List.foldBack (symbolOptionToConsoleChar >> cons) (cells puzzleSize) [ NL ]
    List.iter mainWriter line
    mainWriter NL
    let prows = printRowOnOneLine symbolOptionToConsoleChar puzzleRowCells (Seq.singleton NL) (rows puzzleSize)
    Seq.iter mainWriter prows
    mainWriter NL

    let puzzlePrintGrid = printCells puzzleStacks puzzleStackColumns puzzleBands puzzleBandRows defaultGridChars
    Seq.iter drawConsoleChar (puzzlePrintGrid symbolOptionToConsoleChar)

    let centreCandidate = List.nth candidates ((List.length candidates) / 2)

    let puzzleDrawFull () =
        let puzzleDrawer (cell : Cell) (candidate : Candidate) = 
            drawSymbolCellContentAnnotations centreCandidate candidate ((!solution).start cell) ((!solution).current cell) None

        Seq.iter drawConsoleChar (puzzlePrintFull puzzleDrawer)

    let puzzleDrawFullHint annotations =
        let draw_cell2 (l : Cell -> CellAnnotation) (cell : Cell) (candidate : Candidate) = 
            drawSymbolCellContentAnnotations centreCandidate candidate ((!solution).start cell) ((!solution).current cell) (Some (l cell))

        Seq.iter drawConsoleChar (puzzlePrintFull (draw_cell2 annotations))

    
    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))


    Seq.tryPick (run candidates solution puzzle puzzlePrintGrid puzzlePrintFull puzzleDrawFull puzzleDrawFullHint) readlines |> ignore

let defaultPuzzleSpec = 
    { boxWidth = 3 * 1<width>
      boxHeight = 3 * 1<height>
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Symbol ] }

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
