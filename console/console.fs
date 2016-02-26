module console.console

open System

open core.sudoku
open core.hints

open format

// Things we may want to write
type consoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

let cs = CChar >> Seq.singleton

// Some predefined characters - for smaller grid
let defaultGridChars : gridChars<seq<consoleChar>> = 
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
            r = cs '┘' }
      n = Seq.singleton NL }

// Some predefined characters - for smaller grid
let defaultCandidateGridChars : candidateGridChars<seq<consoleChar>> = 
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
                  r = cs '╝' } }
      n = Seq.singleton NL }

// Change the console colour to write a string
let consoleWriteColor (value : 'a) consoleColour = 
    let foregroundColour = System.Console.ForegroundColor
    try 
        System.Console.ForegroundColor <- consoleColour
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let drawConsoleChar (consoleChar : consoleChar) = 
    match consoleChar with
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar(c, consoleColour) -> consoleWriteColor c consoleColour
    | ColouredString(c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let drawDigitCellContents (firstDigit : digit option) (currentDigit : cellContents) = 
    match firstDigit, currentDigit with
    | Some s, _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None, BigNumber s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellContentAnnotations centreCandidate (candidate : digit) (firstDigit : digit option) 
    (currentDigit : cellContents) (annotationOpt : annotation option) = 
    let isCentre = centreCandidate = candidate

    match firstDigit, currentDigit with
    | Some _, _ when not isCentre -> CChar ' '
    | Some s, _ -> 
        match annotationOpt with
        | Some annotation when annotation.primaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.Cyan)
        | Some annotation when annotation.secondaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.DarkBlue)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Blue)

    | None, BigNumber _ when not isCentre -> CChar ' '
    | None, BigNumber s -> 
        match annotationOpt with
        | Some annotation when annotation.primaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.Yellow)
        | Some annotation when annotation.secondaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.DarkRed)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks candidates -> 
        match annotationOpt with
        | Some annotation when annotation.setValue.IsSome && annotation.setValue.Value = candidate -> 
                ColouredString(candidate.ToString(), ConsoleColor.Red)
        | Some annotation when annotation.setValue.IsSome && Set.contains candidate candidates -> 
            ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some annotation when annotation.setValueReduction.IsSome && annotation.setValueReduction.Value = candidate && Set.contains candidate candidates -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some annotation when Set.contains candidate annotation.reductions -> 
            ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some annotation when Set.contains candidate annotation.pointers -> 
            ColouredString(candidate.ToString(), ConsoleColor.Magenta)
        | Some annotation when Set.contains candidate annotation.focus && Set.contains candidate candidates -> 
            ColouredString(candidate.ToString(), ConsoleColor.Yellow)
        | Some annotation when annotation.primaryHintHouse -> 
            if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
            else CChar ' '
        | Some annotation when annotation.secondaryHintHouse -> 
            if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Green)
            else CChar ' '
        | _ -> 
            if Set.contains candidate candidates then CStr(candidate.ToString())
            else CChar ' '
