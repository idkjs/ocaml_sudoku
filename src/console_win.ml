open Sudoku
open Format
open System
open System.Diagnostics
open System.Runtime.InteropServices

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let cs (c : char) : consoleString = [CChar c]

(* Some predefined characters - for smaller grid *)
let defaultGridChars : gridChars = 
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
      n = [NL] }

(* Some predefined characters - for smaller grid *)
let defaultCandidateGridChars : candidateGridChars = 
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
      n = [NL] }

let basic_to_system (color : basic_color) : ConsoleColor =
    match color with
    | Black -> ConsoleColor.Black
    | Red -> ConsoleColor.Red
    | Green -> ConsoleColor.Green
    | Yellow -> ConsoleColor.Yellow
    | Blue -> ConsoleColor.Blue
    | Magenta -> ConsoleColor.Magenta
    | Cyan -> ConsoleColor.Cyan
    | White -> ConsoleColor.White
    | DarkRed -> ConsoleColor.DarkRed
    | DarkGreen -> ConsoleColor.DarkGreen
    | DarkYellow -> ConsoleColor.DarkYellow
    | DarkBlue -> ConsoleColor.DarkBlue


(* Change the console colour to write a string *)
let consoleWriteColor (consoleColour : ConsoleColor) (value : 'a) = 
    let foregroundColour = System.Console.ForegroundColor in
    try 
        let _ = System.Console.ForegroundColor <- consoleColour in
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let drawConsoleChar (consoleChar : consoleChar) = 
    match consoleChar with
    | CNil -> ()
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | CDigit (Digit d) -> Console.Write d
    | ColouredChar(c, basic_color) -> consoleWriteColor (basic_to_system basic_color) c
    | ColouredString(c, basic_color) -> consoleWriteColor (basic_to_system basic_color) c
    | ColouredDigit ((Digit d), basic_color) -> consoleWriteColor (basic_to_system basic_color) d
    | NL -> Console.WriteLine ""

let drawConsoleString (consoleString : consoleString) : unit =
    List.iter drawConsoleChar consoleString

let maximise_console() : unit =
    let p = Process.GetCurrentProcess() in
    ShowWindow(p.MainWindowHandle, 3) (* SW_MAXIMIZE = 3 *)
    |> ignore
