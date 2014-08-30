module console.console

open System

open core.puzzlemap
open core.sudoku

open format

// Things we may want to write
type ConsoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

let sNL = Seq.singleton NL

let cs = CChar >> Seq.singleton

// Some predefined characters - for smaller grid
let defaultGridChars : gridChars<seq<ConsoleChar>> = 
    { h = cs '─'
      v = 
          { l = cs '│'
            m = cs '│'
            r = cs '│' }
      t = 
          { l = cs '┌'
            m = cs '┬'
            r = cs '┐' }
      m = 
          { l = cs '├'
            m = cs '┼'
            r = cs '┤' }
      b = 
          { l = cs '└'
            m = cs '┴'
            r = cs '┘' } }

// Some predefined characters - for smaller grid
let defaultSolutionChars : solutionChars<seq<ConsoleChar>> = 
    { h = cs '═'
      hi = cs '─'
      v = 
          { l = cs '║'
            m = cs '║'
            r = cs '║' }
      vi = cs '│'
      t = 
          { mi = cs '╦'
            x = 
                { l = cs '╔'
                  m = cs '╦'
                  r = cs '╗' } }
      m = 
          { mi = cs '╬'
            x = 
                { l = cs '╠'
                  m = cs '╬'
                  r = cs '╣' } }
      mi = 
          { mi = cs '┼'
            x = 
                { l = cs '╠'
                  m = cs '╬'
                  r = cs '╣' } }
      b = 
          { mi = cs '╧'
            x = 
                { l = cs '╚'
                  m = cs '╩'
                  r = cs '╝' } } }

// Change the console colour to write a string
let consoleWriteColor (value : 'a) consoleColour = 
    let foregroundColour = System.Console.ForegroundColor
    try 
        System.Console.ForegroundColor <- consoleColour
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let ConsoleWriteChar(consoleChar : ConsoleChar) = 
    match consoleChar with
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar(c, consoleColour) -> consoleWriteColor c consoleColour
    | ColouredString(c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let charToAlphabet (alphabet : Symbol list) (trialSymbol : char) = 
    let compareAlpha (Symbol charSymbol) = trialSymbol = charSymbol
    List.tryFind compareAlpha alphabet

let charToCandidate (alphabet : Candidate list) (trialSymbol : char) = 
    let compareAlpha (Candidate charSymbol) = trialSymbol = charSymbol
    List.tryFind compareAlpha alphabet

let loadLine (line : string) alphabet = List.map (charToAlphabet alphabet) (List.ofSeq line)

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (alphabetisedLine : Symbol option list) (cells : Cell list) cell = 
    let zs = List.zip alphabetisedLine cells
    List.pick (fun (symbolOpt, c) -> 
        if c = cell then Some symbolOpt
        else None) zs

let drawAnnotatedSymbol (asymbol : AnnotatedSymbol<'a>) = 
    match asymbol with
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | Candidates _ -> CChar '.'

let drawAnnotatedSymbolAsHint (asymbol : AnnotatedSymbol<'a>) = 
    match asymbol with
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Green)
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Yellow)
    | Candidates _ -> CChar '.'

let drawHintAnnotatedSymbol (l : HintAnnotatedSymbol) = 
    if l.hintHouse then drawAnnotatedSymbol l.symbol
    else drawAnnotatedSymbolAsHint l.symbol

let drawAnnotatedCandidate (ac : AnnotatedCandidate) (candidate : Candidate) = 
    match ac with
    | Possible -> CStr(candidate.ToString())
    | Excluded -> CChar ' '
    | Removed -> ColouredString(candidate.ToString(), ConsoleColor.DarkMagenta)

let drawHintAnnotatedCandidate (c : AnnotatedCandidate) (candidate : Candidate) = 
    match c with
    | Possible -> ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
    | Excluded -> CChar ' '
    | Removed -> ColouredString(candidate.ToString(), ConsoleColor.DarkMagenta)

let drawHintAnnotatedCandidateHint (hintHouse : bool) (c : Candidate -> HintAnnotatedCandidate) (candidate : Candidate) = 
    match c candidate with
    | HACId l -> 
        if hintHouse then drawHintAnnotatedCandidate l candidate
        else drawAnnotatedCandidate l candidate
    | HACSet -> ColouredString(candidate.ToString(), ConsoleColor.Red)
    | Pointer -> ColouredString(candidate.ToString(), ConsoleColor.Magenta)
    | Reduction -> ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)

let drawFLFE centreCandidate candidate (l : AnnotatedSymbol<AnnotatedCandidate>) = 
    let isCentre = centreCandidate = candidate
    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | Candidates candidates -> drawAnnotatedCandidate (candidates candidate) candidate

let drawHFLFE (hintHouse : bool) centreCandidate candidate (l : AnnotatedSymbol<HintAnnotatedCandidate>) = 
    let isCentre = centreCandidate = candidate
    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | Candidates candidates -> drawHintAnnotatedCandidateHint hintHouse candidates candidate

let drawHintFLFE (hintHouse : bool) centreCandidate candidate (l : AnnotatedSymbol<HintAnnotatedCandidate>) = 
    let isCentre = centreCandidate = candidate
    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Green)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Yellow)
    | Candidates candidates -> drawHintAnnotatedCandidateHint hintHouse candidates candidate

let drawFL2 centreCandidate candidate (l : HintAnnotatedSymbol) = 
    if l.hintHouse then drawHintFLFE true centreCandidate candidate l.symbol
    else drawHFLFE false centreCandidate candidate l.symbol

// Print a symbol option, with colours
let symbolOptionToConsoleChar = 
    function 
    | Some symbol -> Given symbol
    | None -> Candidates(konst Removed)
