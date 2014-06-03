#light

module console

open System
open System.Text

open sudoku
open format
open tactics
open hints


// Things we may want to write
type ConsoleChar =
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | NL

let sNL = Seq.singleton NL

let cs = CChar >> Seq.singleton

// Some predefined characters - for smaller grid

let defaultGridChars:gridChars<seq<ConsoleChar>> = {
    h = cs '─'
    v = cs '│'
    t = { 
        l = cs '┌'
        m = cs '┬'
        r = cs '┐'
        }
    m = {
        l = cs '├'
        m = cs '┼'
        r = cs '┤'
        }
    b = {
        l = cs '└'
        m = cs '┴'
        r = cs '┘'
        }
    nl = sNL
}

// Some predefined characters - for smaller grid

let defaultSolutionChars:solutionChars<seq<ConsoleChar>> = {
    h=cs '═'
    hi=cs '─'
    v=cs '║'
    vi=cs '│'
    t={
        mi=cs '╦'
        x={
            l=cs '╔'
            m=cs '╦'
            r=cs '╗'
        }
    }
    m={
        mi=cs '╬'
        x={
            l=cs '╠'
            m=cs '╬'
            r=cs '╣'
        }
    }
    mi={
        mi=cs '┼'
        x={
            l=cs '╠'
            m=cs '╬'
            r=cs '╣'
        }
    }
    b={
        mi=cs '╧'
        x={
            l=cs '╚'
            m=cs '╩'
            r=cs '╝'
        }
    }
    nl=sNL
}


// Change the console colour to write a string
let consoleWriteColor (value:char) consoleColour =
    let foregroundColour = System.Console.ForegroundColor
    try
        System.Console.ForegroundColor <- consoleColour
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let ConsoleWriteChar = function
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar (c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

// Print a symbol option, with colours
let symbolOptionToConsoleChar = function
    | Some(Symbol s) -> ColouredChar (s, ConsoleColor.Blue)
    | None -> CChar '.'

// Print an entry, with colours
let entryToConsole = function
    | Given(Symbol s) -> ColouredChar (s, ConsoleColor.Blue)
    | Set(Symbol s) -> ColouredChar (s, ConsoleColor.Red)
    | Candidates(_) -> CChar '.'

// Print an entry for a candidate to console
let entryAndCandidateToConsole centreSymbol candidate = function
    | Given(Symbol s) ->
        if centreSymbol = candidate then
            ColouredChar (s, ConsoleColor.Blue)
        else
            CChar ' '
    | Set(Symbol s) ->
        if centreSymbol = candidate then
            ColouredChar (s, ConsoleColor.Red)
        else
            CChar ' '
    | Candidates(candidates) ->
        if Set.contains candidate candidates then
            let (Symbol s) = candidate
            CChar s
        else
            CChar ' '


let loadLine (line:string) alphabet = List.map (charToAlphabet alphabet) (List.ofSeq line)

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (alphabetisedLine:Symbol option list) (cells:Cell list) =

    let grid = new System.Collections.Generic.Dictionary<Cell, Symbol option>()

    List.iter2 (fun cell c -> grid.Add(cell, c) ) cells alphabetisedLine

    let puzzleGridCellLookup = fun cell -> grid.[cell]

    puzzleGridCellLookup



let formatHouse = function
    | Column c ->
        String.Format ("Column {0}", c.col)
    | Row r ->
        String.Format ("Row {0}", r.row)
    | Box b ->
        String.Format ("Box s{0}b{1}", (int)b.stack.stack, (int)b.band.band)

let formatCell cell =
    String.Format ("c{0}r{1}", (int)cell.col.col, (int)cell.row.row)

let formatSymbol (Symbol s) = s

let formatSymbols (candidates:Set<Symbol>) =
    let s = Set.toArray candidates
    let t = Array.map formatSymbol s
    String.Join(",", t)

let formatHiddenSingle {HiddenSingle.cell = cell; symbol = Symbol symbol; house = house} =
    String.Format ("hs {0}, Value {1}, Cell {2}", formatHouse house, symbol, formatCell cell)

let printHiddenSingles hss =
    List.iter (
        fun h ->
            Console.WriteLine (formatHiddenSingle h)
            )
        hss

let hiddenSingleSymbolTo (entryLookup:EntryLookup) (houseCells:HouseCells) (hint:HiddenSingle) (cell:Cell) =
    let hc = getHouseCells houseCells hint.house
    let houseEntryToConsole = function
        | Given(Symbol s) -> ColouredChar (s, ConsoleColor.Green)
        | Set(Symbol s) -> ColouredChar (s, ConsoleColor.Yellow)
        | Candidates(_) -> CChar '.'

    if cell = hint.cell then
        ColouredChar(formatSymbol hint.symbol, ConsoleColor.Cyan)
    else if Set.contains cell (Set.ofList hc) then
        (houseEntryToConsole (entryLookup cell))
    else
        (entryToConsole (entryLookup cell))

let printNakedSingles hints =
    List.iter (
        fun {NakedSingle.cell = cell; symbol = Symbol symbol} ->
                Console.WriteLine ("Cell {0}: Symbol: {1}", formatCell cell, symbol)
            )
        hints

let nakedSingleSymbolTo (entryLookup:EntryLookup) (houseCells:HouseCells) (hint:NakedSingle) (cell:Cell) =
    if cell = hint.cell then
        ColouredChar(formatSymbol hint.symbol, ConsoleColor.Cyan)
    else
        (entryToConsole (entryLookup cell))

let printFullHouse hss =
    List.iter (
        fun hs ->
            match hs with
            | {FullHouse.cell = cell; symbol = Symbol symbol; house = house} ->
                Console.WriteLine ("{0}, Value {1}, Cell {2}", formatHouse house, symbol, formatCell cell)
            )
        hss

let fullHouseSymbolTo (entryLookup:EntryLookup) (houseCells:HouseCells) (hint:FullHouse) (cell:Cell) =
    let hc = getHouseCells houseCells hint.house

    let houseEntryToConsole = function
        | Given(Symbol s) -> ColouredChar (s, ConsoleColor.Green)
        | Set(Symbol s) -> ColouredChar (s, ConsoleColor.Yellow)
        | Candidates(_) -> CChar '.'

    if cell = hint.cell then
        ColouredChar(formatSymbol hint.symbol, ConsoleColor.Cyan)
    else if Set.contains cell (Set.ofList hc) then
        (houseEntryToConsole (entryLookup cell))
    else
        (entryToConsole (entryLookup cell))


let printNakedPairs hints =
    List.iter (
        fun hs ->
            match hs with
            | {NakedPair.cell1 = cell1; cell2 = cell2; symbols = symbols; candidateReduction = candidateReduction; house = house} ->
                Console.WriteLine ("{0}, Cell {1}, Cell {2}, {3}", formatHouse house, formatCell cell1, formatCell cell2, formatSymbols symbols)

                List.iter
                    (fun {CandidateReduction.symbols = candidates; cell = cell} ->
                        Console.WriteLine ("  Cell {0}, Candidates {1}", formatCell cell, formatSymbols candidates)
                    )
                    candidateReduction
            )
        hints

let nakedPairSymbolTo (entryLookup:EntryLookup) (houseCells:HouseCells) (hint:NakedPair) (cell:Cell) chars =
    let hc = getHouseCells houseCells hint.house
    let houseEntryToConsole = function
        | Given(Symbol s) -> ColouredChar (s, ConsoleColor.Green)
        | Set(Symbol s) -> ColouredChar (s, ConsoleColor.Yellow)
        | Candidates(_) -> CChar '.'

    let cc =
        if cell = hint.cell1 || cell = hint.cell2 then
            ColouredChar('a' (*formatSymbol hint.symbol*), ConsoleColor.Cyan)
        else if Set.contains cell (Set.ofList hc) then
            (houseEntryToConsole (entryLookup cell))
        else
            (entryToConsole (entryLookup cell))

    cc :: chars

