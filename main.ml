open Sudoku
open Puzzlemap
open Hint
open Command
open Console
open Format
(*F# open Console_win F#*)
(*IF-OCAML*) open Console_nix (*ENDIF-OCAML*)
(*F# open FSharp.Compatibility.OCaml F#*)

let parse (p : puzzleMap) (item : string) (solution : solution) (puzzle : puzzleShape) 
    (cellCandidates : cellCandidates) puzzleDrawFull2 print_last : solution * Hint.description list = 

    print_endline item;

    if item = "print" then 
        let hd3 = Hint.mhas2 solution p in
        puzzleDrawFull2 hd3.annotations;
        (solution, [])
    else if String.length item >= 5 && String.compare (String.sub item 0 5) "focus" = 0 then
        let focusDigitResult = focusCommandParse puzzle item in
        match focusDigitResult with
        | FCOk (VOk focusDigit) ->
            let hd2 = focusCommandHintDescription p focusDigit in
            let hd3 = Hint.mhas solution p hd2 in
            puzzleDrawFull2 hd3.annotations;
            (solution, [])

        | FCOk r ->
            print_endline (parse_value_result_to_string r);
            (solution, [])

        | FCWrongTermCount _ ->
            print_endline "Expect 'focus <digit>'";
            (solution, [])

    else if item = "load" then
        let candidateReductions = LoadEliminate.find p solution.current in

        let hd2 = LoadEliminate.description p candidateReductions in
        let hd3 = Hint.mhas solution p hd2 in
        puzzleDrawFull2 hd3.annotations;

        let newSolution = LoadEliminate.step p solution candidateReductions in
        (*print_last newSolution*)
        (newSolution, [])
    else if String.get item 0 = 's' then
        let valueOpt = setCellCommandParse puzzle item p in
        
        let newSolution = 
            match valueOpt with
            | SCCOk value ->
                let setCellValueOpt = setCellCommandCheck solution.given cellCandidates value in
                (match setCellValueOpt with
                 | SSCROk setCellValue ->
                    let hd2 = SetCell.description p setCellValue in
                    let hd3 = Hint.mhas solution p hd2 in
                    puzzleDrawFull2 hd3.annotations;

                    SetCell.step p setCellValue solution

                 | SCCRGiven _
                 | SCCRNotACandidate _ ->
                    print_endline (set_cell_command_check_result_to_string setCellValueOpt);
                    solution
                )

            | SCCBadParams (parse_cell, parse_value) ->
                Printf.printf "Cell: %s, Value: %s\n" (parse_cell_results_to_string parse_cell) (parse_value_result_to_string parse_value);
                solution

            | SCCWrongTermCount _ ->
                print_endline "Expect set <col> <row> <val>";
                solution
            in

        print_last newSolution;
        (newSolution, [])

    else if String.get item 0 = 'c' then
        let candidateOpt = candidateClearCommandParse puzzle item p in

        let newSolution = 
            match candidateOpt with
            | CCCPROk candidate ->
                let clearCommandOpt = candidateClearCommandCheck solution.given cellCandidates candidate in
                (match clearCommandOpt with
                 | CCCCROk clearCommand ->
                    let hd2 = EliminateCandidate.description p candidate in
                    let hd3 = Hint.mhas solution p hd2 in
                    puzzleDrawFull2 hd3.annotations;

                    EliminateCandidate.step p candidate solution

                 | CCCCRGiven _
                 | CCCCRNotACandidate _ -> 
                    print_endline (clear_candidate_command_check_result_to_string clearCommandOpt);
                    solution
                )

            | CCCPRParseError (parse_cell, parse_value) ->
                Printf.printf "Cell: %s, Value: %s\n" (parse_cell_results_to_string parse_cell) (parse_value_result_to_string parse_value);
                solution

            | CCCPRWrongItemCount _ ->
                print_endline "Expect clr <col> <row> <val>";
                solution
            in

        print_last newSolution;
        (newSolution, [])

    else
        let supportedHintOpt = Smap.tryGet String.compare supportedHints item in
        match supportedHintOpt with
        | Some supportedHint ->
            let hints = supportedHint p cellCandidates in
            (solution, hints)
        | None -> (solution, [])

let printHint (p : puzzleMap) drawHint (solution : solution) (index : int) (hint : Hint.description) : unit = 

    Printf.printf "%d: %s\n" index (Hint.Description.to_string hint);

    let hd3 = Hint.mhas solution p hint in
    drawHint hd3.annotations;

    read_line() |> ignore

let run (solution : solution ref) (puzzle : puzzleShape) 
    puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint item : string option = 
    if item = "quit" then Some "quit"
    else
        let p = tPuzzleMap puzzle in

        let cellCandidates = Solution.currentCellCandidates p.cells (!solution).current in

        let (soln, hints) = 
            parse p item !solution puzzle cellCandidates puzzleDrawCandidateGridAnnotations print_last in
        solution := soln;

        List.iteri (puzzlePrintHint soln) hints;

        None

let repl (sudoku : string) (puzzleShape : puzzleShape) : unit = 

    print_endline sudoku;

    let p = tPuzzleMap puzzleShape in

    let solution = ref (Load.load puzzleShape sudoku) in

    let centreDigit : digit = Digits.nth puzzleShape.alphabet ((Digits.count puzzleShape.alphabet) / 2) in

    (* Print a Digit option, with colours *)
    let puzzleDrawCell (solution : solution) (cell : cell) : consoleString = 
        drawDigitCellString (Given.get solution.given cell) (Current.get solution.current cell)
        in

    let puzzleDrawLine (solution : solution) : unit =
        printLine p.cells (puzzleDrawCell solution)
        |> drawConsoleString
        in

    let puzzleDrawGrid (solution : solution) : unit =
        printGrid p defaultGridChars (puzzleDrawCell solution)
        |> drawConsoleString
        in

    let puzzleDrawCandidateGridAnnotations annotations : unit = 
        printCandidateGrid p defaultCandidateGridChars puzzleShape.alphabet (drawDigitCellContentAnnotationString centreDigit annotations)
        |> drawConsoleString
        in

    let print_last (solution : solution) : unit = 
        puzzleDrawGrid solution;

        match solution.steps with
        | action :: _ -> 
            (match action with
             | Load _ -> drawConsoleChar (CStr "");
             | LoadEliminate  -> drawConsoleChar (CStr "");
             | Placement sv -> drawConsoleChar (CStr (Value.to_string sv));
             | Eliminate candidate -> drawConsoleChar (CStr(Candidate.to_string candidate));
            )
        | [] -> ();

        drawConsoleChar NL;
        in

    puzzleDrawLine !solution;
    drawConsoleChar NL;
    puzzleDrawGrid !solution;

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

    let puzzlePrintHint = printHint p puzzleDrawCandidateGridAnnotations in

    let rec loop (l : string list) : string list =
        print_string ">";
        let item : string = read_line() in
        let r : string option = run solution puzzleShape puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint item in
        match r with
        | Some s -> l
        | None -> loop (item::l)
        in
    loop []
    |> ignore;;

maximise_console();

(*
let all_core_tests : (unit -> unit) list = Test_core.all_tests in
all_core_tests
|> List.iter (fun test -> test());
*)

(* Input puzzle *)
print_endline "1........2........3........4........5........6........7........8........9........";
print_endline "123456789123456789123456789123456789123456789123456789123456789123456789123456789";

(*let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800" *)
(*let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" *)

(* FullHouse *)
(*let example = "800739006370465000040182009000600040054300610060500000400853070000271064100940002"*)
(*let example = "801006094300009080970080500547062030632000050198375246083620915065198000219500008"*)
let example = "2...3..7.9...1..8.5...6.9.4653871492489325761721496.....5.8.....6..4.....9..5...3" in

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

Printf.printf "\033[38;5;208mpeach\033[0;00m\n";
Printf.printf "\027[31;1mbright red\027[39;49;0m\n";
Printf.printf "\027[31;7minvert red\027[39;49;0m\n";
Printf.printf "\027[31mred\027[39;49;0m\n";

repl example PuzzleShape.default';

print_endline "bye"
