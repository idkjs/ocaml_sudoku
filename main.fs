module main

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

open Sudoku
open Puzzlemap

open Command
open Console
open format

(*F# open FSharp.Compatibility.OCaml F#*)

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) (* SW_MAXIMIZE = 3 *)

let parse (p : puzzleMap) (item : string) (solution : solution) (puzzle : puzzleShape) 
    (cellCandidates : cellCandidates) puzzleDrawFull2 print_last : solution * Hint.description list = 

    Console.WriteLine item

    if item = "print" then 
        let hd3 = Hint.mhas2 solution p
        puzzleDrawFull2 hd3.annotations
        (solution, List.empty)
    else if item.StartsWith "focus" then
        let focusDigitResult = focusCommandParse puzzle item
        match focusDigitResult with
        | FCOk (VOk focusDigit) ->
            let hd2 = focusCommandHintDescription p focusDigit
            let hd3 = Hint.mhas solution p hd2
            puzzleDrawFull2 hd3.annotations
            (solution, List.empty)

        | FCOk r ->
            Console.WriteLine(parse_value_result_to_string r)
            (solution, List.empty)

        | FCWrongTermCount _ ->
            Console.WriteLine("Expect 'focus <digit>'")
            (solution, List.empty)

    else if item = "load" then
        let candidateReductions = LoadEliminate.find p solution.current

        let hd2 = LoadEliminate.description p candidateReductions
        let hd3 = Hint.mhas solution p hd2
        puzzleDrawFull2 hd3.annotations

        let newSolution = LoadEliminate.step p solution candidateReductions
        //print_last newSolution
        (newSolution, List.empty)
    else if item.StartsWith "s" then
        let valueOpt = setCellCommandParse puzzle item p
        
        let newSolution = 
            match valueOpt with
            | SCCOk value ->
                let setCellValueOpt = setCellCommandCheck solution.given cellCandidates value
                match setCellValueOpt with
                | SSCROk setCellValue ->
                    let hd2 = SetCell.description p setCellValue
                    let hd3 = Hint.mhas solution p hd2
                    puzzleDrawFull2 hd3.annotations

                    SetCell.step p setCellValue solution

                | SCCRGiven _
                | SCCRNotACandidate _ ->
                    Console.WriteLine (set_cell_command_check_result_to_string setCellValueOpt)
                    solution

            | SCCBadParams (parse_cell, parse_value) ->
                Console.WriteLine(String.Format("Cell: {0}, Value: {1}", (parse_cell_results_to_string parse_cell), (parse_value_result_to_string parse_value)))
                solution

            | SCCWrongTermCount _ ->
                Console.WriteLine "Expect set <col> <row> <val>"
                solution
            in

        print_last newSolution
        (newSolution, List.empty)

    else if item.StartsWith "c" then
        let candidateOpt = candidateClearCommandParse puzzle item p

        let newSolution = 
            match candidateOpt with
            | CCCPROk candidate ->
                let clearCommandOpt = candidateClearCommandCheck solution.given cellCandidates candidate
                match clearCommandOpt with
                | CCCCROk clearCommand ->
                    let hd2 = EliminateCandidate.description p candidate
                    let hd3 = Hint.mhas solution p hd2
                    puzzleDrawFull2 hd3.annotations

                    EliminateCandidate.step p candidate solution

                | CCCCRGiven _
                | CCCCRNotACandidate _ -> 
                    Console.WriteLine(clear_candidate_command_check_result_to_string clearCommandOpt)
                    solution

            | CCCPRParseError (parse_cell, parse_value) ->
                Console.WriteLine(String.Format("Cell: {0}, Value: {1}", (parse_cell_results_to_string parse_cell), (parse_value_result_to_string parse_value)))
                solution

            | CCCPRWrongItemCount _ ->
                Console.WriteLine "Expect clr <col> <row> <val>"
                solution

        print_last newSolution
        (newSolution, List.empty)

    else
        let supportedHintOpt = Smap.tryGet String.compare supportedHints item
        match supportedHintOpt with
        | Some supportedHint ->
            let hints = supportedHint p cellCandidates
            (solution, hints)
        | None -> (solution, List.empty)

let printHint (solution : solution) (p : puzzleMap) drawHint (index : int) (hint : Hint.description) : unit = 

    Console.WriteLine("{0}: {1}", index, hint)

    let hd3 = Hint.mhas solution p hint
    drawHint hd3.annotations

    Console.Read() |> ignore

let run (solution : solution ref) (puzzle : puzzleShape) 
    puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint item = 
    if item = "quit" then Some "quit"
    else
        let p = tPuzzleMap puzzle

        let cellCandidates = Solution.currentCellCandidates p.cells (!solution).current

        let (soln, hints) = 
            parse p item !solution puzzle cellCandidates puzzleDrawCandidateGridAnnotations print_last
        solution := soln

        Seq.iteri puzzlePrintHint hints

        None


let repl (sudoku : string) (puzzleShape : puzzleShape) = 

    Console.WriteLine sudoku

    let p = tPuzzleMap puzzleShape

    let solution = ref (Load.load puzzleShape sudoku)

    let centreDigit : digit = Digits.nth puzzleShape.alphabet ((Digits.count puzzleShape.alphabet) / 2)

    (* Print a Digit option, with colours *)
    let puzzleDrawCell (solution : solution) (cell : cell) : consoleChar = 
        drawDigitCellContents (Given.get solution.given cell) (Current.get solution.current cell)

    let puzzleDrawLine (solution : solution) =
        Seq.iter drawConsoleChar (printLine p.cells (puzzleDrawCell solution))

    let puzzleDrawGrid (solution : solution) =
        Seq.iter drawConsoleChar (printGrid p defaultGridChars (puzzleDrawCell solution))
    
    let puzzleDrawCandidateGridAnnotations annotations = 
        Seq.iter drawConsoleChar (printCandidateGrid p defaultCandidateGridChars puzzleShape.alphabet (drawDigitCellContentAnnotations centreDigit annotations))

    let print_last (solution : solution) = 
        puzzleDrawGrid solution

        match solution.steps with
        | action :: _ -> 
            match action with
            | Load _ -> drawConsoleChar (CStr "")
            | LoadEliminate  -> drawConsoleChar (CStr "")
            | Placement sv -> drawConsoleChar (CStr (Value.to_string sv))
            | Eliminate candidate -> drawConsoleChar (CStr(Candidate.to_string candidate))
        | [] -> ()

        drawConsoleChar NL

    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))

    puzzleDrawLine !solution
    drawConsoleChar NL

    puzzleDrawGrid !solution

(*
    let stopwatch = new Stopwatch() in
    let _ = stopwatch.Start() in

    let forcedSolutions = Force.solve p (!solution)

    let _ = stopwatch.Stop() in

    let _ = Console.WriteLine("Time elapsed: {0}", stopwatch.Elapsed) in

    //puzzleDrawGrid() |> ignore

    if List.length forcedSolutions > 0 then
        forcedSolutions
        |> List.iter
            (fun solve ->
                Seq.iter drawConsoleChar (printGrid p defaultGridChars (puzzleDrawCell solve)))

    else Console.WriteLine("No solutions")
*)

    let puzzlePrintHint = printHint (!solution) p puzzleDrawCandidateGridAnnotations

    Seq.tryPick (run solution puzzleShape puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint) 
        readlines |> ignore

Maximize() |> ignore

let all_core_tests = Test_core.all_tests in

all_core_tests
|> List.iter (fun test -> test())


(* Input puzzle *)
Console.WriteLine "1........2........3........4........5........6........7........8........9........"
Console.WriteLine "123456789123456789123456789123456789123456789123456789123456789123456789123456789"

(*let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800" *)
(*let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" *)

(* FullHouse *)
(*let example = "800739006370465000040182009000600040054300610060500000400853070000271064100940002"*)
(*let example = "801006094300009080970080500547062030632000050198375246083620915065198000219500008"*)
let example = "2...3..7.9...1..8.5...6.9.4653871492489325761721496.....5.8.....6..4.....9..5...3"

(* ht *)
(*let example = "528600049136490025794205630000100200007826300002509060240300976809702413070904582" *)
(* hq *)
(*let example = "...3742......82.4..............3.8266...9...48.5.4697.547.2...9......4.5.1.45.7.2" *)
(* http://www.sudokuwiki.org/Hidden_Candidates hq *)
(*let example = "65..87.24...649.5..4..25...57.438.61...5.1...31.9.2.85...89..1....213...13.75..98" *)
(*let example = "000500000425090001800010020500000000019000460000000002090040003200060807000001600" *)

(* http://www.sudokuwiki.org/X_Wing_Strategy *)
(* let example = "100000569492056108056109240009640801064010000218035604040500016905061402621000005" *)
(* http://www.sudokuwiki.org/Y_Wing_Strategy *)
(*let example = "900240000050690231020050090090700320002935607070002900069020073510079062207086009" *)
(*let example = "273005081810302004009010200100953728792186345538724196021060500300201869080530412" *)


repl example PuzzleShape.default'

Console.WriteLine "bye"
