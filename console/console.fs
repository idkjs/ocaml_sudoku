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

let drawAnnotatedCandidate (ac : AnnotatedCandidate) (candidate : Candidate) = 
    match ac with
    | Possible -> CStr(candidate.ToString())
    | Excluded -> CChar ' '
    | Removed -> CChar ' '

let drawFLFE centreCandidate candidate (l : AnnotatedSymbol<AnnotatedCandidate>) = 

    let isCentre = centreCandidate = candidate

    match l with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | Candidates candidates -> drawAnnotatedCandidate (candidates candidate) candidate

let drawFL2 centreCandidate candidate (l : HintAnnotatedSymbol) = 

    let isCentre = centreCandidate = candidate

    match l.symbol with
    | Given _ when not isCentre -> CChar ' '
    | Given s -> 
        if l.primaryHintHouse then ColouredString(s.ToString(), ConsoleColor.Cyan)
        else if l.secondaryHintHouse then ColouredString(s.ToString(), ConsoleColor.DarkBlue)
        else ColouredString(s.ToString(), ConsoleColor.Blue)
    | Set _ when not isCentre -> CChar ' '
    | Set s -> 
        if l.primaryHintHouse then ColouredString(s.ToString(), ConsoleColor.Yellow)
        else if l.secondaryHintHouse then ColouredString(s.ToString(), ConsoleColor.DarkRed)
        else ColouredString(s.ToString(), ConsoleColor.Red)
    | Candidates candidates -> 
        match candidates candidate with
        | HACId h -> 
            if l.primaryHintHouse then 
                match h with
                | Possible -> ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
                | Excluded -> CChar ' '
                | Removed -> CChar ' '
            else if l.secondaryHintHouse then 
                match h with
                | Possible -> ColouredString(candidate.ToString(), ConsoleColor.Green)
                | Excluded -> CChar ' '
                | Removed -> CChar ' '
            else drawAnnotatedCandidate h candidate
        | HACSet -> ColouredString(candidate.ToString(), ConsoleColor.Red)
        | Pointer -> ColouredString(candidate.ToString(), ConsoleColor.Magenta)
        | Reduction -> ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)

// Print a symbol option, with colours
let symbolOptionToConsoleChar = 
    function 
    | Some symbol -> Given symbol
    | None -> Candidates(konst Removed)
