module console.console

open System

open core.sudoku
open core.hints

open format

// Things we may want to write
type ConsoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

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
            r = cs '┘' }
      n = Seq.singleton NL }

// Some predefined characters - for smaller grid
let defaultCandidateGridChars : candidateGridChars<seq<ConsoleChar>> = 
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

let drawConsoleChar (consoleChar : ConsoleChar) = 
    match consoleChar with
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar(c, consoleColour) -> consoleWriteColor c consoleColour
    | ColouredString(c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let drawDigitCellContents (firstDigit : Digit option) (currentDigit : CellContents) = 
    match firstDigit, currentDigit with
    | Some s, _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None, BigNumber s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellContentAnnotations centreCandidate (focusDigit : Digit option) (candidate : Digit) (firstDigit : Digit option) 
    (currentDigit : CellContents) (currentHintDigitOpt : CellAnnotation option) = 
    let isCentre = centreCandidate = candidate

    match firstDigit, currentDigit with
    | Some _, _ when not isCentre -> CChar ' '
    | Some s, _ -> 
        let isFocus =
            match focusDigit with
            | Some d when d = s -> true
            | Some _ -> false
            | None -> true

        if isFocus then
            match currentHintDigitOpt with
            | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
                ColouredString(s.ToString(), ConsoleColor.Cyan)
            | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
                ColouredString(s.ToString(), ConsoleColor.DarkBlue)
            | _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
        else ColouredString(s.ToString(), ConsoleColor.Gray)
    | None, BigNumber _ when not isCentre -> CChar ' '
    | None, BigNumber s -> 
        match currentHintDigitOpt with
        | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.Yellow)
        | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.DarkRed)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks candidates -> 
        let isFocus =
            match focusDigit with
            | Some d when d = candidate -> true
            | Some _ -> false
            | None -> true

        if focusDigit.IsNone then
            match currentHintDigitOpt with
            | Some currentHintDigit when currentHintDigit.setValue.IsSome && currentHintDigit.setValue.Value = candidate -> 
                    ColouredString(candidate.ToString(), ConsoleColor.Red)
            | Some currentHintDigit when currentHintDigit.setValue.IsSome && Set.contains candidate candidates -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when currentHintDigit.setValueReduction.IsSome && currentHintDigit.setValueReduction.Value = candidate && Set.contains candidate candidates -> 
                    ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when Set.contains candidate currentHintDigit.reductions -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when Set.contains candidate currentHintDigit.pointers -> 
                ColouredString(candidate.ToString(), ConsoleColor.Magenta)
            | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
                else CChar ' '
            | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Green)
                else CChar ' '
            | _ -> 
                if Set.contains candidate candidates then CStr(candidate.ToString())
                else CChar ' '
        else
            if focusDigit.Value = candidate then
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Blue)
                else CChar ' '
            else
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Gray)
                else CChar ' '
