module console.command

open System

open core.sudoku
open core.puzzlemap
open core.hints
open core.setCell
open core.eliminateCandidate

open hints.fullHouse
open hints.hidden
open hints.intersection
open hints.naked
open hints.wing

open console.console
open console.format

// find a column or row
let parseColumnRow what gridSize term = 
    match Int32.TryParse term with
    | (true, col) when col >= 1 && col <= gridSize -> Some col
    | _ -> 
        Console.WriteLine("{0} must be a number between 1 and {1}", what, gridSize)
        None

// find a cell from a pair of strings
let parseCell (gridSize : int) (cells : Set<cell>) (termColumn : string) (termRow : string) : cell option =
    let parsedCol = parseColumnRow "Column" gridSize termColumn
    let parsedRow = parseColumnRow "Row" gridSize termRow

    match (parsedCol, parsedRow) with
    | (Some col, Some row) ->
        cells
        |> Set.toList
        |> List.tryFind (fun cell -> cell.col = (makeColumn col) && cell.row = (makeRow row))
    | _ -> 
        Console.WriteLine("({0},{1} is not a cell", termColumn, termRow)
        None

let charToCandidate (candidates : digit list) (trialDigit : char) = 
    let compareAlpha (Digit charDigit) = trialDigit = charDigit
    List.tryFind compareAlpha candidates

// find an element of the alphabet
let parseValue (candidates : digit list) (term : string) = 
    if term.Length = 1 then charToCandidate candidates (term.Chars 0)
    else 
        Console.WriteLine("Expect a single digit, not {0}", term)
        None

let focusCommandParse (s: puzzleShape) (item : string) : digit option =
    let terms = item.Split(' ')
    if terms.Length = 2 then 
        parseValue s.alphabet terms.[1]
    else
        None

let focusCommandHintDescription (p : puzzleMap) (digit : digit) : hintDescription =
    let hd = 
        { hintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set []
          setCellValueAction = None
          pointers = set []
          focus = set [digit] }

    hd

let setCellCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : value option = 
    let terms = item.Split(' ')
    if terms.Length = 4 then 
        let parsedCell = parseCell s.alphabet.Length p.cells terms.[1] terms.[2]
        let parsedValue = parseValue s.alphabet terms.[3]

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) -> 
            makeSetCellDigit cell value
            |> Some
        | _ -> None
    else None

let setCellCommandCheck (given : given) (cellCandidates : cellCandidates) (value : value) : value option =
    let givenDigitOpt = given.Item value.cell
    match givenDigitOpt with
    | Some givenDigit ->
        Console.WriteLine("Error: Cell {0} has given {1}", value.cell, givenDigit)
        None
    | None ->
        let digits = cellCandidates.Get value.cell
        if Set.contains value.digit digits then Some value
        else
            Console.WriteLine("Warning: Cell {0} does not have candidate {1}", value.cell, value.digit)
            value
            |> Some


let candidateClearCommandParse (s: puzzleShape) (item : string) (p : puzzleMap) : candidate option = 
    let terms = item.Split(' ')
    if terms.Length = 4 then 
        let parsedCell = parseCell s.alphabet.Length p.cells terms.[1] terms.[2]
        let parsedDigit = parseValue s.alphabet terms.[3]

        match (parsedCell, parsedDigit) with
        | (Some cell, Some digit) ->
            makeCandidate cell digit
            |> Some
        | _ -> 
            None
    else 
        None

let candidateClearCommandCheck (given : given) (cellCandidates : cellCandidates) (candidate : candidate) : candidate option =
    let givenDigitOpt = given.Item candidate.cell
    match givenDigitOpt with
    | Some givenDigit ->
        Console.WriteLine("Error: Cell {0} has given {1}", candidate.cell, givenDigit)
        None
    | None ->
        let digits = cellCandidates.Get candidate.cell
        if Set.contains candidate.digit digits then Some candidate
        else
            Console.WriteLine("Warning: Cell {0} does not have candidate {1}", candidate.cell, candidate.digit)
            candidate
            |> Some

let SupportedHints : Map<string, puzzleMap -> cellCandidates -> Set<hintDescription>> =
    [
        ("fh", fullHouses)
        ("hs", hiddenN 1)
        ("hp", hiddenN 2)
        ("ht", hiddenN 3)
        ("hq", hiddenN 4)
        ("ns", nakedSingle)
        ("np", nakedN 2)
        ("nt", nakedN 3)
        ("nq", nakedN 4)
        ("pp", pointingPairs)
        ("bl", boxLineReductions)
        ("x",  xWings)
        ("y",  yWings)
    ]
    |> Map.ofList
