#light

module command

open System
open System.Text

open sudoku
open format
open console
open tactics

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
        findCell (col * 1<col>) (row * 1<row>) cells
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



let symbolsToCandidates (symbolLookup : SymbolLookup) houseCells entry cell =
    match entry with
    | Candidates(c1) ->
        let c = 
            candidateSymbols cell symbolLookup houseCells c1
        Candidates(c)
    | _ -> entry

let makeSolutionGrid s cells =

    let s3 = List.map2 (fun a b -> (a, b)) cells s

    let s2 = s3 |> Map.ofList

    let solutionGrid = new System.Collections.Generic.Dictionary<Cell, Entry>(s2)

    solutionGrid

// Update the set of candidates
let updateCandidates (entryLookup:EntryLookup) houseCells (cells:Cell list) c value =

    let houses = allHouseCells c houseCells

    List.map (
        fun cell ->
            let entry = entryLookup cell

            match entry with
            | Candidates(c1) when houses.Contains cell -> Candidates(Set.remove value c1)
            | _ -> entry
    ) cells

// Update the set of candidates
let doSet (entryLookup:EntryLookup) (cells:Cell list) c value =
    List.map (
        fun cell ->
            if cell = c then
                Set(value)
            else
                entryLookup cell
    ) cells

let setValue (entryLookup:EntryLookup) houseCells (cells:Cell list) cell value =
    let c = entryLookup(cell)
    match c with
    | Given(Symbol s) ->
        Console.WriteLine ("Cell <{0}, {1}> has given value {2}", cell.col, cell.row, s)
        None
    | Set(Symbol s) ->
        Console.WriteLine ("Cell <{0}, {1}> has been set value {2}", cell.col, cell.row, s)
        None
    | Candidates(_) ->
        let lookup cell2 =
            if cell = cell2 then
                Set(value)
            else
                entryLookup(cell2)

        let updatedGrid3 = doSet entryLookup cells cell value

        let updatedGrid4 = makeSolutionGrid updatedGrid3 cells

        let entryLookup2 = solutionGridCellLookup updatedGrid4

        let updatedGrid = updateCandidates entryLookup2 houseCells cells cell value

        let updatedGrid2 = makeSolutionGrid updatedGrid cells

        let action = SetValue(cell, value)

        let step = { grid = (solutionGridCellLookup updatedGrid2); action = action }

        Some step

let ui_set (item:string) gridSize alphabet (lastGrid:EntryLookup) houseCells (cells:Cell list) =
    let terms = item.Split(' ')
    if terms.Length = 4 then
        let parsedCell = parseCell gridSize cells terms.[1] terms.[2]
        let parsedValue = parseValue alphabet terms.[3]

        match (parsedCell, parsedValue) with
        | (Some cell, Some value) ->
            setValue lastGrid houseCells cells cell value
        | _ ->
            Console.WriteLine "Expect set <col> <row> <val>"
            None
    else
        Console.WriteLine "Expect set <col> <row> <val>"
        None

