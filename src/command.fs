module command

open System

open Smap
open Sudoku
open Puzzlemap
open hints
open SetCell
open EliminateCandidate

open FullHouse
open Hidden
open Intersection
open Naked
open Wing

open console
open format

(* find a column or row *)
let parseColumnRow what gridSize term = 
    match Int32.TryParse term with
    | (true, col) when col >= 1 && col <= gridSize -> Some col
    | _ -> 
        Console.WriteLine("{0} must be a number between 1 and {1}", what, gridSize)
        None

(* find a cell from a pair of strings *)
let parseCell (gridSize : int) (cells : cell list) (termColumn : string) (termRow : string) : cell option =
    let parsedCol = parseColumnRow "Column" gridSize termColumn in
    let parsedRow = parseColumnRow "Row" gridSize termRow in

    match (parsedCol, parsedRow) with
    | (Some col, Some row) ->
        cells
        |> List.tryFind (fun cell -> cell.col = (makeColumn col) && cell.row = (makeRow row))
    | _ -> 
        Console.WriteLine("({0},{1} is not a cell", termColumn, termRow)
        None

let charToCandidate (digits : digit list) (trialDigit : char) = 
    let compareAlpha (Digit charDigit) = trialDigit = charDigit in
    List.tryFind compareAlpha digits

(* find an element of the alphabet *)
let parseValue (digits : digit list) (term : string) = 
    if term.Length = 1 then charToCandidate digits (term.Chars 0)
    else 
        Console.WriteLine("Expect a single digit, not {0}", term)
        None

let focusCommandParse (s: puzzleShape) (item : string) : digit option =
    let terms = item.Split(' ') in
    if terms.Length = 2 then 
        parseValue s.alphabet terms.[1]
    else
        None

let focusCommandHintDescription (p : puzzleMap) (digit : digit) : hintDescription =
    { hintDescription.primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = CandidateReductions.empty;
      setCellValueAction = None;
      pointers = CandidateReductions.empty;
      focus = Digits.singleton digit }

let setCellCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : value option = 
    let terms = item.Split(' ') in
    if terms.Length = 4 then 
        let parsedCell = parseCell s.alphabet.Length p.cells terms.[1] terms.[2] in
        let parsedValue = parseValue s.alphabet terms.[3] in

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) -> Some (makeValue cell value)
        | _ -> None
    else None

let setCellCommandCheck (given : given) (cellCandidates : cellCandidates) (value : value) : value option =
    let givenDigitOpt = SMap.get given value.cell in
    match givenDigitOpt with
    | Some givenDigit ->
        Console.WriteLine("Error: Cell {0} has given {1}", value.cell, givenDigit)
        None
    | None ->
        let digits = SMap.get cellCandidates value.cell in
        if Digits.contains value.digit digits then Some value
        else
            Console.WriteLine("Warning: Cell {0} does not have candidate {1}", value.cell, value.digit)
            value
            |> Some


let candidateClearCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : candidate option = 
    let terms = item.Split(' ') in
    if terms.Length = 4 then 
        let parsedCell = parseCell s.alphabet.Length p.cells terms.[1] terms.[2] in
        let parsedDigit = parseValue s.alphabet terms.[3] in

        match (parsedCell, parsedDigit) with
        | (Some cell, Some digit) -> Some (makeCandidate cell digit)
        | _ -> None
    else 
        None

let candidateClearCommandCheck (given : given) (cellCandidates : cellCandidates) (candidate : candidate) : candidate option =
    let givenDigitOpt = SMap.get given candidate.cell in
    match givenDigitOpt with
    | Some givenDigit ->
        Console.WriteLine("Error: Cell {0} has given {1}", candidate.cell, givenDigit)
        None
    | None ->
        let digits = SMap.get cellCandidates candidate.cell in
        if Digits.contains candidate.digit digits then Some candidate
        else
            Console.WriteLine("Warning: Cell {0} does not have candidate {1}", candidate.cell, candidate.digit)
            candidate
            |> Some

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
