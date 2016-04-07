open Sudoku
open Puzzlemap

open Console
open Format

(*F# open FSharp.Compatibility.OCaml F#*)

type parse_column_or_row_results =
    | CROk of int
    | CRError of string * int

let parse_column_or_row_results_to_string (r : parse_column_or_row_results) : string =
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

let parse_cell_results_to_string (r : parse_cell_results) : string =
    match r with
    | COk cell -> Cell.to_string cell
    | CColError (parsedCol, row) -> Printf.sprintf "(%s,%d) column wrong, is not a cell" (parse_column_or_row_results_to_string parsedCol) row
    | CRowError (col, parsedRow) -> Printf.sprintf "(%d,%s) row wrong, is not a cell" col (parse_column_or_row_results_to_string parsedRow)
    | CColRowError (parsedCol, parsedRow) -> Printf.sprintf "(%s,%s) column and row wrong, is not a cell" (parse_column_or_row_results_to_string parsedCol) (parse_column_or_row_results_to_string parsedRow)

(* find a cell from a pair of strings *)
let parseCell (gridSize : int) (cells : cells) (termColumn : string) (termRow : string) : parse_cell_results =
    let parsedCol = parseColumnRow "Column" gridSize termColumn in
    let parsedRow = parseColumnRow "Row" gridSize termRow in

    match (parsedCol, parsedRow) with
    | (CROk col, CROk row) ->
        cells
        |> Cells.toList
        |> List.find (fun cell -> cell.col = (Column.make col) && cell.row = (Row.make row))
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

let parse_value_result_to_string (r : parse_value_result) : string =
    match r with
    | VOk d -> Printf.sprintf "%s" (Digit.to_string d)
    | VErrorInvalid (term, digits) -> Printf.sprintf "%s must be one of %s" term digits
    | VErrorTooMany term -> Printf.sprintf "%s should be single character" term

(* find an element of the alphabet *)
let parseValue (digits : digits) (term : string) : parse_value_result = 
    if term.Length = 1 then
        match charToCandidate digits (term.Chars 0) with
        | Some d -> VOk d
        | None -> VErrorInvalid (term, (Digits.to_string digits))
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

let focusCommandHintDescription (p : puzzleMap) (digit : digit) : Hint.description =
    { primaryHouses = Houses.empty;
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
        | (COk cell, VOk value) -> SCCOk (Value.make cell value)
        | _ -> SCCBadParams (parsedCell, parsedValue)
    else SCCWrongTermCount (terms.Length)

type set_cell_command_check_result =
    | SSCROk of value
    | SCCRGiven of value * digit
    | SCCRNotACandidate of value

let set_cell_command_check_result_to_string (r : set_cell_command_check_result) : string =
    match r with
    | SSCROk value -> ""
    | SCCRGiven (value, digit) -> Printf.sprintf "Error: Cell %s has given %s" (Cell.to_string value.cell) (Digit.to_string digit)
    | SCCRNotACandidate value -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (Cell.to_string value.cell) (Digit.to_string value.digit)

let setCellCommandCheck (given : given) (cellCandidates : cellCandidates) (value : value) : set_cell_command_check_result =
    let givenDigitOpt = Given.get given value.cell in
    match givenDigitOpt with
    | Some givenDigit -> SCCRGiven (value, givenDigit)
    | None ->
        let digits = CellCandidates.get cellCandidates value.cell in
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
        | (COk cell, VOk digit) -> CCCPROk (Candidate.make cell digit)
        | _ -> CCCPRParseError (parsedCell, parsedDigit)
    else CCCPRWrongItemCount (terms.Length)

type clear_candidate_command_check_result =
    | CCCCROk of candidate
    | CCCCRGiven of candidate * digit
    | CCCCRNotACandidate of candidate

let clear_candidate_command_check_result_to_string (r : clear_candidate_command_check_result) : string =
    match r with
    | CCCCROk candidate -> ""
    | CCCCRGiven (candidate, digit) -> Printf.sprintf "Error: Cell %s has given %s" (Cell.to_string candidate.cell) (Digit.to_string digit)
    | CCCCRNotACandidate candidate -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (Cell.to_string candidate.cell) (Digit.to_string candidate.digit)

let candidateClearCommandCheck (given : given) (cellCandidates : cellCandidates) (candidate : candidate) : clear_candidate_command_check_result =
    let givenDigitOpt = Given.get given candidate.cell in
    match givenDigitOpt with
    | Some givenDigit -> CCCCRGiven (candidate, givenDigit)
    | None ->
        let digits = CellCandidates.get cellCandidates candidate.cell in
        if Digits.contains candidate.digit digits then CCCCROk candidate
        else CCCCRNotACandidate candidate

let supportedHints : (string * (puzzleMap -> cellCandidates -> Hint.description list)) list =
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
        | "fh" -> FullHouse.find
        | "hs" -> Hidden.find 1
        | "hp" -> Hidden.find 2
        | "ht" -> Hidden.find 3
        | "hq" -> Hidden.find 4
        | "ns" -> Naked.find 1
        | "np" -> Naked.find 2
        | "nt" -> Naked.find 3
        | "nq" -> Naked.find 4
        | "pp" -> Intersection.pointingPairs
        | "bl" -> Intersection.boxLineReductions
        | "x" -> Wing.xWings
        | "y" -> Wing.yWings
        in

    Smap.ofLookup keys command
