module console.command

open System

open console.console
open console.format
open core.eliminateCandidate
open core.setCell
open core.sudoku
open core.puzzlemap

// find a column or row
let parseColumnRow what gridSize term = 
    match Int32.TryParse term with
    | (true, col) when col >= 1 && col <= gridSize -> Some col
    | _ -> 
        Console.WriteLine("{0} must be a number between 1 and {1}", what, gridSize)
        None

// find a cell from a pair of strings
let parseCell gridSize cells termColumn termRow = 
    let parsedCol = parseColumnRow "Column" gridSize termColumn
    let parsedRow = parseColumnRow "Row" gridSize termRow

    match (parsedCol, parsedRow) with
    | (Some col, Some row) -> 
        List.tryFind (fun cell -> cell.col.col = col * 1<column> && cell.row.row = row * 1<row>) cells
    | _ -> 
        Console.WriteLine("({0},{1} is not a cell", termColumn, termRow)
        None

let charToCandidate (candidates : Digit list) (trialDigit : char) = 
    let compareAlpha (Digit charDigit) = trialDigit = charDigit
    List.tryFind compareAlpha candidates

// find an element of the alphabet
let parseValue (candidates : Digit list) (term : string) = 
    if term.Length = 1 then charToCandidate candidates (term.Chars 0)
    else 
        Console.WriteLine("Expect a single digit, not {0}", term)
        None

let setCellCommand (item : string) (alphabet : Digit list) (lastGrid : Current) (cells : Cell list) 
    (cellHouseCells : MapCellHouseCells) (candidateLookup : MapCellCandidates) : Value option = 
    let terms = item.Split(' ')
    if terms.Length = 4 then 
        let parsedCell = parseCell alphabet.Length cells terms.[1] terms.[2]
        let parsedValue = parseValue alphabet terms.[3]

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) -> 
            let either = setCellDigitTry cell value lastGrid
            match either with
            | Left setCellDigitAction -> Some setCellDigitAction
            | Right setCellDigitError -> 
                Console.WriteLine("Cell {0} has been set value {1}", cell, setCellDigitError.digit)
                None
        | _ -> 
            Console.WriteLine "Expect set <col> <row> <val>"
            None
    else 
        Console.WriteLine "Expect set <col> <row> <val>"
        None

let candidateClearCommand (item : string) (alphabet : Digit list) (lastGrid : Current) 
    (cells : Cell list) : Candidate option = 
    let terms = item.Split(' ')
    if terms.Length = 4 then 
        let parsedCell = parseCell alphabet.Length cells terms.[1] terms.[2]
        let parsedCandidate = parseValue alphabet terms.[3]

        match (parsedCell, parsedCandidate) with
        | (Some cell, Some candidate) -> 
            match eliminateCandidateTry cell candidate lastGrid with
            | Left eliminateCandidateAction -> Some eliminateCandidateAction
            | Right(AlreadySet digit) -> 
                Console.WriteLine("Cell {0} has been set value {1}", cell, digit)
                None
            | Right NotACandidate -> 
                Console.WriteLine("Cell {0} does not have candidate {1}", cell, candidate)
                None
        | _ -> 
            Console.WriteLine "Expect clr <col> <row> <val>"
            None
    else 
        Console.WriteLine "Expect clr <col> <row> <val>"
        None
