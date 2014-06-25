module command

open System
open System.Text

open sudoku
open puzzlemap
open format
open console

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
let parseValue alphabet (term:string) =
    if term.Length = 1 then
        charToAlphabet alphabet (term.Chars 0)
    else
        Console.WriteLine ("Expect a single digit, not {0}", term)
        None


type SetCellValue =
    {
        cell:Cell
        value:Symbol
        housesCells:Set<Cell>
    }

// Modify entryLoopup to set a cell to a value
let setACell (entryLookup:EntryLookup) (setCellValue:SetCellValue) : EntryLookup =
    fun cell ->
        if setCellValue.cell = cell then
            Set(setCellValue.value)
        else
            let entry = entryLookup cell

            match entry with
            | Candidates candidates when setCellValue.housesCells.Contains cell -> Candidates(Set.remove setCellValue.value candidates)
            | _ -> entry



let setValue (entryLookup:EntryLookup) (puzzleMaps:PuzzleMaps) cell value =
    let entry = entryLookup cell
    match entry with
    | Given(Symbol s) ->
        Console.WriteLine ("Cell {0} has given value {1}", formatCell cell, s)
        None
    | Set(Symbol s) ->
        Console.WriteLine ("Cell {0} has been set value {1}", formatCell cell, s)
        None
    | Candidates(_) ->
        let housesCells = allHouseCells puzzleMaps cell

        let setCellValue =
            {
                SetCellValue.cell = cell
                value = value
                housesCells = housesCells
            }

        let sac = setACell entryLookup setCellValue

        let action = SetValue(cell, value)

        Some (sac, action)

let ui_set (item:string) (alphabet:Alphabet) (lastGrid:EntryLookup) (puzzleMaps:PuzzleMaps) =
    let terms = item.Split(' ')
    if terms.Length = 4 then
        let parsedCell = parseCell alphabet.Length puzzleMaps.cells terms.[1] terms.[2]
        let parsedValue = parseValue alphabet terms.[3]

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) ->
            setValue lastGrid puzzleMaps cell value
        | _ ->
            Console.WriteLine "Expect set <col> <row> <val>"
            None
    else
        Console.WriteLine "Expect set <col> <row> <val>"
        None

