module console

open System
open System.Text

open sudoku
open puzzlemap
open format
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
    v = {
        l = cs '│'
        m = cs '│'
        r = cs '│'
        }
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
}

// Some predefined characters - for smaller grid

let defaultSolutionChars:solutionChars<seq<ConsoleChar>> = {
    h=cs '═'
    hi=cs '─'
    v={
        l=cs '║'
        m=cs '║'
        r=cs '║'
      }
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
}


// Change the console colour to write a string
let consoleWriteColor (value:'a) consoleColour =
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

let charToAlphabet (alphabet : Alphabet) (trialSymbol : char) = 
  let compareAlpha symbol =
    let (Symbol charSymbol) = symbol
    trialSymbol = charSymbol

  List.tryFind compareAlpha alphabet

let loadLine (line:string) alphabet = List.map (charToAlphabet alphabet) (List.ofSeq line)

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (alphabetisedLine:Symbol option list) (cells:Cell list) cell =
    let zs = List.zip alphabetisedLine cells

    List.pick (fun (symbolOpt, c) ->
        if c = cell then
            Some symbolOpt
        else
            None) zs

let formatColumn (c:Column) = String.Format ("Column {0}", c.col)

let formatRow (r:Row) = String.Format ("Row {0}", r.row)

let formatBox (b:Box) = String.Format ("Box s{0}b{1}", (int)b.stack.stack, (int)b.band.band)

let formatHouse = function
    | Column c -> formatColumn c
    | Row r -> formatRow r
    | Box b -> formatBox b

let formatCell cell = String.Format ("c{0}r{1}", (int)cell.col.col, (int)cell.row.row)

let formatSymbol (Symbol s) = s

let formatSymbols (candidates:Set<Symbol>) =
    let s = Set.toArray candidates
    let t = Array.map formatSymbol s
    String.Join(",", t)


let drawFL (l:FormatLabel) =
    match l with
    | LPlain (Given s) -> ColouredChar (formatSymbol s, ConsoleColor.Blue)
    | LPlain (Set s) -> ColouredChar (formatSymbol s, ConsoleColor.Red)
    | LPlain (Candidates _) -> CChar '.'
    | LHintHouse (Given s) -> ColouredChar (formatSymbol s, ConsoleColor.Green)
    | LHintHouse (Set s) -> ColouredChar (formatSymbol s, ConsoleColor.Yellow)
    | LHintHouse (Candidates _) -> CChar '.'
    | LHintCell s -> ColouredChar(formatSymbol s, ConsoleColor.Cyan)

let drawFLF (l:FormatLabelF) =
    match l with
    | FLGiven s -> ColouredChar (formatSymbol s, ConsoleColor.Blue)
    | FLSet s -> ColouredChar (formatSymbol s, ConsoleColor.Red)
    | FLCandidatePossible s -> CChar (formatSymbol s)
    | FLCandidateExcluded s -> CChar ' '
    | FLHintHouseGiven s -> ColouredChar (formatSymbol s, ConsoleColor.Green)
    | FLHintHouseSet s -> ColouredChar (formatSymbol s, ConsoleColor.Yellow)
    | FLHintCell s -> ColouredChar(formatSymbol s, ConsoleColor.Cyan)
    | FLHintCandidatePointer s -> ColouredChar (formatSymbol s, ConsoleColor.Magenta)
    | FLHintCandidateReduction s -> ColouredChar (formatSymbol s, ConsoleColor.DarkYellow)

let drawFL2 centreCandidate candidate (l:FormatLabelF) =
    let c = centreCandidate = candidate

    match l with
    | FLGiven _ when not c -> CChar ' '
    | FLSet _ when not c -> CChar ' '
    | FLCandidatePossible symbol ->
                                CChar (formatSymbol symbol)
    | FLCandidateExcluded _ ->
                                CChar ' '
    | FLHintHouseGiven _ when not c -> CChar ' '
    | FLHintHouseSet _ when not c -> CChar ' '
    | FLHintCell symbol -> ColouredChar(formatSymbol symbol, ConsoleColor.Cyan)
    | FLHintCandidatePointer symbol -> ColouredChar (formatSymbol symbol, ConsoleColor.Magenta)
    | FLHintCandidateReduction symbol -> ColouredChar (formatSymbol symbol, ConsoleColor.DarkYellow)
    | _ -> drawFLF (l)

// Print a symbol option, with colours
let symbolOptionToConsoleChar = function
    | Some symbol -> LPlain (Given symbol)
    | None -> LPlain (Candidates Set.empty)

// Print an entry, with colours
let entryToConsole (entry:Entry) = LPlain entry

// Print an entry for a candidate to console
let entryAndCandidateToConsole candidate entry =
    match entry with
    | Given symbol -> FLGiven symbol
    | Set symbol -> FLSet symbol
    | Candidates candidates ->
        if Set.contains candidate candidates then
            FLCandidatePossible candidate
        else
            FLCandidateExcluded candidate


