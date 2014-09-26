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

let drawAnnotatedSymbol (firstSymbol : Symbol option) (currentSymbol : CellContents) = 
    match firstSymbol with
    | Some s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None _ -> 
        match currentSymbol with
        | ASymbol s -> ColouredString(s.ToString(), ConsoleColor.Red)
        | ACandidates _ -> CChar '.'

let drawFLFE centreCandidate candidate (firstSymbol : Symbol option) (currentSymbol : CellContents) = 

    let isCentre = centreCandidate = candidate

    match firstSymbol with
    | Some _ when not isCentre -> CChar ' '
    | Some s -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None -> 
        match currentSymbol with
        | ASymbol _ when not isCentre -> CChar ' '
        | ASymbol s -> ColouredString(s.ToString(), ConsoleColor.Red)
        | ACandidates candidates -> 
            if Set.contains candidate candidates then CStr(candidate.ToString())
            else CChar ' '

let drawFL2 centreCandidate candidate (firstSymbol : Symbol option) (currentSymbol : CellContents) (currentHintSymbol : CellAnnotation) = 
    let isCentre = centreCandidate = candidate

    match firstSymbol with
    | Some _ when not isCentre -> CChar ' '
    | Some s -> 
        if currentHintSymbol.primaryHintHouse then ColouredString(s.ToString(), ConsoleColor.Cyan)
        else if currentHintSymbol.secondaryHintHouse then ColouredString(s.ToString(), ConsoleColor.DarkBlue)
        else ColouredString(s.ToString(), ConsoleColor.Blue)
    | None -> 
        match currentSymbol with
        | ASymbol _ when not isCentre -> CChar ' '
        | ASymbol s -> 
            if currentHintSymbol.primaryHintHouse then ColouredString(s.ToString(), ConsoleColor.Yellow)
            else if currentHintSymbol.secondaryHintHouse then ColouredString(s.ToString(), ConsoleColor.DarkRed)
            else ColouredString(s.ToString(), ConsoleColor.Red)
        | ACandidates previousCandidates -> 
            match currentSymbol with
            | ASymbol s -> ColouredString(s.ToString(), ConsoleColor.Red)
            | ACandidates currentCandidates -> 
                if currentHintSymbol.setValue.IsSome && currentHintSymbol.setValue.Value = candidate then 
                    ColouredString(candidate.ToString(), ConsoleColor.Red)
                else if Set.contains candidate currentHintSymbol.reductions then 
                    ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
                else if Set.contains candidate currentHintSymbol.pointers then 
                    ColouredString(candidate.ToString(), ConsoleColor.Magenta)
                else if currentHintSymbol.primaryHintHouse then 
                    if Set.contains candidate currentCandidates then 
                        ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
                    else CChar ' '
                else if currentHintSymbol.secondaryHintHouse then 
                    if Set.contains candidate currentCandidates then 
                        ColouredString(candidate.ToString(), ConsoleColor.Green)
                    else CChar ' '
                else if Set.contains candidate currentCandidates then CStr(candidate.ToString())
                else CChar ' '
