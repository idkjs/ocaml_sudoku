module command

open System
open System.Text

open sudoku
open puzzlemap
open format
open console
open setCell

// find a column or row
let parseColumnRow what gridSize term =
    match Int32.TryParse term with
    | (true, col) when col >= 1 && col <= gridSize ->
        Some col
    | _ ->
        Console.WriteLine ("{0} must be a number between 1 and {1}", what, gridSize)
        None

// find a cell from a pair of strings
let parseCell gridSize cells termColumn termRow =
    let parsedCol = parseColumnRow "Column" gridSize termColumn
    let parsedRow = parseColumnRow "Row" gridSize termRow

    match (parsedCol, parsedRow) with
    | (Some col, Some row) ->
        List.tryFind
            (fun cell -> cell.col.col = col * 1<col> && cell.row.row = row * 1<row>)
            cells
    | _ ->
        Console.WriteLine("({0},{1} is not a cell", termColumn, termRow)
        None

// find an element of the alphabet
let parseValue (alphabet:Candidate list) (term:string) =
    if term.Length = 1 then
        charToCandidate alphabet (term.Chars 0)
    else
        Console.WriteLine ("Expect a single digit, not {0}", term)
        None

let ui_set (item:string) (alphabet:Candidate list) (lastGrid:Cell->AnnotatedSymbol<AnnotatedCandidate>) (puzzleMaps:PuzzleMaps) =
    let terms = item.Split(' ')
    if terms.Length = 4 then
        let parsedCell = parseCell alphabet.Length puzzleMaps.cells terms.[1] terms.[2]
        let parsedValue = parseValue alphabet terms.[3]

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) ->
            setValue puzzleMaps value lastGrid cell
        | _ ->
            Console.WriteLine "Expect set <col> <row> <val>"
            None
    else
        Console.WriteLine "Expect set <col> <row> <val>"
        None


