module command

open Smap
open Sudoku
open Puzzlemap
open Hints
open SetCell
open EliminateCandidate

open FullHouse
open Hidden
open Intersection
open Naked
open Wing

open console
open format

(*F# open FSharp.Compatibility.OCaml F#*)

type parse_column_or_row_results =
    | CROk of int
    | CRError of string * int

let parse_column_or_row_results_tostring (r : parse_column_or_row_results) : string =
    match r with
    | CROk i -> Printf.sprintf "%d" i
    | CRError (what, gridSize) -> Printf.sprintf "%s must be a number between 1 and %d" what gridSize

(* find a column or row *)
let parseColumnRow what gridSize term : parse_column_or_row_results =
    try
        let i = int_of_string term in
        if i >= 1 && i <= gridSize then CROk i
        else CRError (what, gridSize)
    with
        | Failure e -> CRError (what, gridSize)

type parse_cell_results = 
    | COk of cell
    | CColError of parse_column_or_row_results * int
    | CRowError of int * parse_column_or_row_results
    | CColRowError of parse_column_or_row_results * parse_column_or_row_results

let parse_cell_results_tostring (r : parse_cell_results) : string =
    match r with
    | COk cell -> cell_tostring cell
    | CColError (parsedCol, row) -> Printf.sprintf "(%s,%d) column wrong, is not a cell" (parse_column_or_row_results_tostring parsedCol) row
    | CRowError (col, parsedRow) -> Printf.sprintf "(%d,%s) row wrong, is not a cell" col (parse_column_or_row_results_tostring parsedRow)
    | CColRowError (parsedCol, parsedRow) -> Printf.sprintf "(%s,%s) column and row wrong, is not a cell" (parse_column_or_row_results_tostring parsedCol) (parse_column_or_row_results_tostring parsedRow)

(* find a cell from a pair of strings *)
let parseCell (gridSize : int) (cells : cells) (termColumn : string) (termRow : string) : parse_cell_results =
    let parsedCol = parseColumnRow "Column" gridSize termColumn in
    let parsedRow = parseColumnRow "Row" gridSize termRow in

    match (parsedCol, parsedRow) with
    | (CROk col, CROk row) ->
        cells
        |> Cells.toList
        |> List.find (fun cell -> cell.col = (makeColumn col) && cell.row = (makeRow row))
        |> COk
    | (CRError _, CROk row) -> CColError (parsedCol, row)
    | (CROk col, CRError _) -> CRowError (col, parsedRow)
    | (CRError _, CRError _) -> CColRowError (parsedCol, parsedRow)

let charToCandidate (digits : digits) (trialDigit : char) : digit option = 
    let compareAlpha (Digit charDigit) = trialDigit = charDigit in
    List.tryFind compareAlpha (Digits.toList digits)

type parse_value_result =
    | VOk of digit
    | VErrorInvalid of string * string
    | VErrorTooMany of string

let parse_value_result_tostring (r : parse_value_result) : string =
    match r with
    | VOk d -> Printf.sprintf "%s" (digit_tostring d)
    | VErrorInvalid (term, digits) -> Printf.sprintf "%s must be one of %s" term digits
    | VErrorTooMany term -> Printf.sprintf "%s should be single character" term

(* find an element of the alphabet *)
let parseValue (digits : digits) (term : string) : parse_value_result = 
    if term.Length = 1 then
        match charToCandidate digits (term.Chars 0) with
        | Some d -> VOk d
        | None -> VErrorInvalid (term, (Digits.tostring digits))
    else VErrorTooMany term

type focus_command_result =
    | FCOk of parse_value_result
    | FCWrongTermCount of int

let focusCommandParse (s: puzzleShape) (item : string) : focus_command_result =
    let terms = item.Split(' ') in
    if terms.Length = 2 then 
        parseValue s.alphabet terms.[1]
        |> FCOk
    else
        FCWrongTermCount terms.Length

let focusCommandHintDescription (p : puzzleMap) (digit : digit) : hintDescription =
    { hintDescription.primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = [];
      setCellValueAction = None;
      pointers = [];
      focus = Digits.singleton digit }

type set_cell_command_parse_result =
    | SCCOk of value
    | SCCBadParams of parse_cell_results * parse_value_result
    | SCCWrongTermCount of int

let setCellCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : set_cell_command_parse_result = 
    let terms = item.Split(' ') in
    if terms.Length = 4 then 
        let parsedCell = parseCell (Digits.count s.alphabet) p.cells terms.[1] terms.[2] in
        let parsedValue = parseValue s.alphabet terms.[3] in

        match (parsedCell, parsedValue) with
        | (COk cell, VOk value) -> SCCOk (makeValue cell value)
        | _ -> SCCBadParams (parsedCell, parsedValue)
    else SCCWrongTermCount (terms.Length)

type set_cell_command_check_result =
    | SSCROk of value
    | SCCRGiven of value * digit
    | SCCRNotACandidate of value

let set_cell_command_check_result_tostring (r : set_cell_command_check_result) : string =
    match r with
    | SSCROk value -> ""
    | SCCRGiven (value, digit) -> Printf.sprintf "Error: Cell %s has given %s" (cell_tostring value.cell) (digit_tostring digit)
    | SCCRNotACandidate value -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (cell_tostring value.cell) (digit_tostring value.digit)

let setCellCommandCheck (given : given) (cellCandidates : cellCandidates) (value : value) : set_cell_command_check_result =
    let givenDigitOpt = SMap.get given value.cell in
    match givenDigitOpt with
    | Some givenDigit -> SCCRGiven (value, givenDigit)
    | None ->
        let digits = SMap.get cellCandidates value.cell in
        if Digits.contains value.digit digits then SSCROk value
        else SCCRNotACandidate value

type clear_candidate_command_parse_result =
    | CCCPROk of candidate
    | CCCPRParseError of parse_cell_results * parse_value_result
    | CCCPRWrongItemCount of int

let candidateClearCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : clear_candidate_command_parse_result = 
    let terms = item.Split(' ') in
    if terms.Length = 4 then 
        let parsedCell = parseCell (Digits.count s.alphabet) p.cells terms.[1] terms.[2] in
        let parsedDigit = parseValue s.alphabet terms.[3] in

        match (parsedCell, parsedDigit) with
        | (COk cell, VOk digit) -> CCCPROk (makeCandidate cell digit)
        | _ -> CCCPRParseError (parsedCell, parsedDigit)
    else CCCPRWrongItemCount (terms.Length)

type clear_candidate_command_check_result =
    | CCCCROk of candidate
    | CCCCRGiven of candidate * digit
    | CCCCRNotACandidate of candidate

let clear_candidate_command_check_result_tostring (r : clear_candidate_command_check_result) : string =
    match r with
    | CCCCROk candidate -> ""
    | CCCCRGiven (candidate, digit) -> Printf.sprintf "Error: Cell %s has given %s" (cell_tostring candidate.cell) (digit_tostring digit)
    | CCCCRNotACandidate candidate -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (cell_tostring candidate.cell) (digit_tostring candidate.digit)

let candidateClearCommandCheck (given : given) (cellCandidates : cellCandidates) (candidate : candidate) : clear_candidate_command_check_result =
    let givenDigitOpt = SMap.get given candidate.cell in
    match givenDigitOpt with
    | Some givenDigit -> CCCCRGiven (candidate, givenDigit)
    | None ->
        let digits = SMap.get cellCandidates candidate.cell in
        if Digits.contains candidate.digit digits then CCCCROk candidate
        else CCCCRNotACandidate candidate

let supportedHints : SMap<string, (puzzleMap -> cellCandidates -> hintDescription list)> =
    let keys =
        [
            "fh";
            "hs";
            "hp";
            "ht";
            "hq";
            "ns";
            "np";
            "nt";
            "nq";
            "pp";
            "bl";
            "x";
            "y";
        ]
        in

    let command key =
        match key with
        | "fh" -> fullHouses
        | "hs" -> hiddenN 1
        | "hp" -> hiddenN 2
        | "ht" -> hiddenN 3
        | "hq" -> hiddenN 4
        | "ns" -> nakedSingle
        | "np" -> nakedN 2
        | "nt" -> nakedN 3
        | "nq" -> nakedN 4
        | "pp" -> pointingPairs
        | "bl" -> boxLineReductions
        | "x" -> xWings
        | "y" -> yWings
        in

    SMap.ofLookup keys command
