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
    | Some s, _ -> ColouredString(Digit.to_string s, ConsoleColor.Blue)
    | None, BigNumber s -> ColouredString(Digit.to_string s, ConsoleColor.Red)
    | None, PencilMarks _ -> CChar '.'

let drawBigNumber (annotation' : annotation) (s : digit) : consoleChar =
    match annotation' with
    | annotation when annotation.primaryHintHouse && annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, ConsoleColor.Cyan)
    | annotation when annotation.primaryHintHouse -> 
        ColouredString(Digit.to_string s, ConsoleColor.Yellow)
    | annotation when annotation.secondaryHintHouse && annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, ConsoleColor.DarkBlue)
    | annotation when annotation.secondaryHintHouse -> 
        ColouredString(Digit.to_string s, ConsoleColor.DarkRed)
    | annotation when annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, ConsoleColor.Blue)
    | _ -> ColouredString(Digit.to_string s, ConsoleColor.Red)

let drawPencilMarks (annotation' : annotation) (candidate : digit) (candidates : digits) : consoleChar =
    match annotation' with
    | annotation when annotation.setValue.IsSome && annotation.setValue.Value = candidate -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.Red)
    | annotation when annotation.setValue.IsSome && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.DarkYellow)
    | annotation when annotation.setValueReduction.IsSome && annotation.setValueReduction.Value = candidate && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.DarkYellow)
    | annotation when Digits.contains candidate annotation.reductions -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.DarkYellow)
    | annotation when Digits.contains candidate annotation.pointers -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.Magenta)
    | annotation when Digits.contains candidate annotation.focus && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, ConsoleColor.Yellow)
    | annotation when annotation.primaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, ConsoleColor.DarkGreen)
        else CChar ' '
    | annotation when annotation.secondaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, ConsoleColor.Green)
        else CChar ' '
    | _ -> 
        if Digits.contains candidate candidates then CStr(Digit.to_string candidate)
        else CChar ' '

let drawDigitCellContentAnnotations centreCandidate (annotations : (cell * annotation) list) (cell : cell) (candidate : digit) : consoleChar = 

    let annotation' = SMap.get annotations cell in
    let isCentre = centreCandidate = candidate in

    match annotation'.current with
    | BigNumber _ when not isCentre -> CChar ' '
    | BigNumber s -> drawBigNumber annotation' s
    | PencilMarks digits -> drawPencilMarks annotation' candidate digits
