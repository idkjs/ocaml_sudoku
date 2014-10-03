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

let ConsoleWriteChar (consoleChar : ConsoleChar) = 
    match consoleChar with
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar(c, consoleColour) -> consoleWriteColor c consoleColour
    | ColouredString(c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let drawAnnotatedSymbol (firstSymbol : Symbol option) (currentSymbol : CellContents) = 
    match firstSymbol, currentSymbol with
    | Some s, _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None, ASymbol s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, ACandidates _ -> CChar '.'

let drawFL2 centreCandidate candidate (firstSymbol : Symbol option) (currentSymbol : CellContents) (currentHintSymbolOpt : CellAnnotation option) = 
    let isCentre = centreCandidate = candidate

    match firstSymbol, currentSymbol with
    | Some _, _ when not isCentre -> CChar ' '
    | Some s, _ -> 
        match currentHintSymbolOpt with
        | Some currentHintSymbol when currentHintSymbol.primaryHintHouse -> ColouredString(s.ToString(), ConsoleColor.Cyan)
        | Some currentHintSymbol when currentHintSymbol.secondaryHintHouse -> ColouredString(s.ToString(), ConsoleColor.DarkBlue)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None, ASymbol _ when not isCentre -> CChar ' '
    | None, ASymbol s -> 
        match currentHintSymbolOpt with
        | Some currentHintSymbol when currentHintSymbol.primaryHintHouse -> ColouredString(s.ToString(), ConsoleColor.Yellow)
        | Some currentHintSymbol when currentHintSymbol.secondaryHintHouse -> ColouredString(s.ToString(), ConsoleColor.DarkRed)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, ACandidates candidates -> 
        match currentHintSymbolOpt with
        | Some currentHintSymbol when currentHintSymbol.setValue.IsSome && currentHintSymbol.setValue.Value = candidate -> 
                ColouredString(candidate.ToString(), ConsoleColor.Red)
        | Some currentHintSymbol when currentHintSymbol.setValue.IsSome && Set.contains candidate candidates ->
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some currentHintSymbol when currentHintSymbol.setValueReduction.IsSome && currentHintSymbol.setValueReduction.Value = candidate && Set.contains candidate candidates -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some currentHintSymbol when Set.contains candidate currentHintSymbol.reductions -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
        | Some currentHintSymbol when Set.contains candidate currentHintSymbol.pointers -> 
                ColouredString(candidate.ToString(), ConsoleColor.Magenta)
        | Some currentHintSymbol when currentHintSymbol.primaryHintHouse -> 
                if Set.contains candidate candidates then 
                    ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
                else CChar ' '
        | Some currentHintSymbol when currentHintSymbol.secondaryHintHouse -> 
                if Set.contains candidate candidates then 
                    ColouredString(candidate.ToString(), ConsoleColor.Green)
                else CChar ' '
        | _ ->
            if Set.contains candidate candidates then CStr(candidate.ToString())
            else CChar ' '
