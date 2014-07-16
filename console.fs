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

let charToAlphabet (alphabet : Symbol list) (trialSymbol : char) = 
  let compareAlpha (Symbol charSymbol) =
    trialSymbol = charSymbol

  List.tryFind compareAlpha alphabet

let charToCandidate (alphabet : Candidate list) (trialSymbol : char) = 
  let compareAlpha (Candidate charSymbol) =
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

let formatCandidate (Candidate c) = c

let formatSymbols (candidates:Set<Symbol>) =
    let s = Set.toArray candidates
    let t = Array.map formatSymbol s
    String.Join(",", t)

let formatCandidates (candidates:Set<Candidate>) =
    let s = Set.toArray candidates
    let t = Array.map formatCandidate s
    String.Join(",", t)

let formatCandidateReduction (candidateReduction:CandidateReduction) =
    String.Format ("Cell {0}, Candidates {1}", formatCell candidateReduction.cell, formatCandidates candidateReduction.symbols)

let drawAnnotatedSymbol (asymbol:AnnotatedSymbol<'a>) =
    match asymbol with
    | Given s -> ColouredChar (formatSymbol s, ConsoleColor.Blue)
    | Set s -> ColouredChar (formatSymbol s, ConsoleColor.Red)
    | Candidates _ -> CChar '.'

let drawAnnotatedSymbolAsHint (asymbol:AnnotatedSymbol<'a>) =
    match asymbol with
    | Given s -> ColouredChar (formatSymbol s, ConsoleColor.Green)
    | Set s -> ColouredChar (formatSymbol s, ConsoleColor.Yellow)
    | Candidates _ -> CChar '.'

let drawHintAnnotatedSymbol (l:HintAnnotatedSymbol) =
    match l with
    | HASId entry -> drawAnnotatedSymbol entry
    | HASHId entry -> drawAnnotatedSymbol entry
    | HASHint entry -> drawAnnotatedSymbolAsHint entry

let drawAnnotatedCandidate (ac:AnnotatedCandidate) (candidate:Candidate) =
    match ac with
    | Possible -> CChar (formatCandidate candidate)
    | Excluded -> CChar ' '
    | Removed -> ColouredChar (formatCandidate candidate, ConsoleColor.DarkMagenta)

let drawHintAnnotatedCandidate (c:AnnotatedCandidate) (candidate:Candidate) =
    match c with
    | Possible -> ColouredChar (formatCandidate candidate, ConsoleColor.DarkGreen)
    | Excluded -> CChar ' '
    | Removed -> ColouredChar (formatCandidate candidate, ConsoleColor.DarkMagenta)

let drawHintAnnotatedCandidateHint (c:Candidate->HintAnnotatedCandidate) (candidate:Candidate) =
    match c candidate with
    | HACId l -> drawAnnotatedCandidate l candidate
    | HACSet -> ColouredChar (formatCandidate candidate, ConsoleColor.Red)
    | Pointer -> ColouredChar (formatCandidate candidate, ConsoleColor.Magenta)
    | Reduction -> ColouredChar (formatCandidate candidate, ConsoleColor.DarkYellow)
    | HACHouse l -> drawHintAnnotatedCandidate l candidate

let drawFLFE centreCandidate candidate (l:AnnotatedSymbol<AnnotatedCandidate>) =
    let isCentre = centreCandidate = candidate

    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredChar (formatSymbol s, ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredChar (formatSymbol s, ConsoleColor.Red)
    | Candidates candidates -> drawAnnotatedCandidate (candidates candidate) candidate

let drawHFLFE centreCandidate candidate (l:AnnotatedSymbol<HintAnnotatedCandidate>) =
    let isCentre = centreCandidate = candidate

    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredChar (formatSymbol s, ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredChar (formatSymbol s, ConsoleColor.Red)
    | Candidates candidates -> drawHintAnnotatedCandidateHint candidates candidate


let drawHintFLFE centreCandidate candidate (l:AnnotatedSymbol<HintAnnotatedCandidate>) =
    let isCentre = centreCandidate = candidate

    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredChar (formatSymbol s, ConsoleColor.Green)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredChar (formatSymbol s, ConsoleColor.Yellow)
    | Candidates candidates -> drawHintAnnotatedCandidateHint candidates candidate

let drawFL2 centreCandidate candidate (l:HintAnnotatedSymbol) =
    match l with
    | HASId entry -> drawFLFE centreCandidate candidate entry
    | HASHId entry -> drawHFLFE centreCandidate candidate entry
    | HASHint entry -> drawHintFLFE centreCandidate candidate entry

// Print a symbol option, with colours
let symbolOptionToConsoleChar = function
    | Some symbol -> Given symbol
    | None -> Candidates (konst Removed)
