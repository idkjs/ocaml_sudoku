module console

open System

open Smap
open Sudoku
open Hints

open format

(* Things we may want to write *)
type consoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

let cs = CChar >> Seq.singleton

(* Some predefined characters - for smaller grid *)
let defaultGridChars : gridChars<seq<consoleChar>> = 
    { h = cs '─';
      v = 
          { l = cs '│';
            m = cs '│';
            r = cs '│' };
      t = 
          { l = cs '┌';
            m = cs '┬';
            r = cs '┐' };
      m = 
          { l = cs '├';
            m = cs '┼';
            r = cs '┤' };
      b = 
          { l = cs '└';
            m = cs '┴';
            r = cs '┘' };
      n = Seq.singleton NL }

(* Some predefined characters - for smaller grid *)
let defaultCandidateGridChars : candidateGridChars<seq<consoleChar>> = 
    { h = cs '═';
      hi = cs '─';
      v = 
          { l = cs '║';
            m = cs '║';
            r = cs '║' };
      vi = cs '│';
      t = 
          { mi = cs '╦';
            x = 
                { l = cs '╔';
                  m = cs '╦';
                  r = cs '╗' } };
      m = 
          { mi = cs '╬';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      mi = 
          { mi = cs '┼';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      b = 
          { mi = cs '╧';
            x = 
                { l = cs '╚';
                  m = cs '╩';
                  r = cs '╝' } };
      n = Seq.singleton NL }

(* Change the console colour to write a string *)
let consoleWriteColor (value : 'a) consoleColour = 
    let foregroundColour = System.Console.ForegroundColor in
    try 
        let _ = System.Console.ForegroundColor <- consoleColour in
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

let drawBigNumber annotation' s : consoleChar =
    match annotation' with
    | annotation when annotation.primaryHintHouse && annotation.given.IsSome -> 
        ColouredString(s.ToString(), ConsoleColor.Cyan)
    | annotation when annotation.primaryHintHouse -> 
        ColouredString(s.ToString(), ConsoleColor.Yellow)
    | annotation when annotation.secondaryHintHouse && annotation.given.IsSome -> 
        ColouredString(s.ToString(), ConsoleColor.DarkBlue)
    | annotation when annotation.secondaryHintHouse -> 
        ColouredString(s.ToString(), ConsoleColor.DarkRed)
    | annotation when annotation.given.IsSome -> 
        ColouredString(s.ToString(), ConsoleColor.Blue)
    | _ -> ColouredString(s.ToString(), ConsoleColor.Red)

let drawPencilMarks annotation' candidate candidates : consoleChar =
    match annotation' with
    | annotation when annotation.setValue.IsSome && annotation.setValue.Value = candidate -> 
        ColouredString(candidate.ToString(), ConsoleColor.Red)
    | annotation when annotation.setValue.IsSome && Digits.contains candidate candidates -> 
        ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
    | annotation when annotation.setValueReduction.IsSome && annotation.setValueReduction.Value = candidate && Digits.contains candidate candidates -> 
        ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
    | annotation when Digits.contains candidate annotation.reductions -> 
        ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
    | annotation when Digits.contains candidate annotation.pointers -> 
        ColouredString(candidate.ToString(), ConsoleColor.Magenta)
    | annotation when Digits.contains candidate annotation.focus && Digits.contains candidate candidates -> 
        ColouredString(candidate.ToString(), ConsoleColor.Yellow)
    | annotation when annotation.primaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
        else CChar ' '
    | annotation when annotation.secondaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Green)
        else CChar ' '
    | _ -> 
        if Digits.contains candidate candidates then CStr(candidate.ToString())
        else CChar ' '

let drawDigitCellContentAnnotations centreCandidate (annotations : SMap<cell, annotation>) (cell : cell) (candidate : digit) : consoleChar = 

    let annotation' = SMap.get annotations cell in
    let isCentre = centreCandidate = candidate in

    match annotation'.current with
    | BigNumber _ when not isCentre -> CChar ' '
    | BigNumber s -> drawBigNumber annotation' s
    | PencilMarks digits -> drawPencilMarks annotation' candidate digits
