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
    | DefaultColour -> ConsoleColor.White
    | Black -> ConsoleColor.Black
    | Red -> ConsoleColor.Red
    | Green -> ConsoleColor.Green
    | Yellow -> ConsoleColor.Yellow
    | Blue -> ConsoleColor.Blue
    | Magenta -> ConsoleColor.Magenta
    | Cyan -> ConsoleColor.Cyan
    | White -> ConsoleColor.White

(* Change the console colour to write a string *)
let consoleWriteColor (foreground_colour : basic_color) (background_colour : basic_color) (value : 'a) = 
    match foreground_colour, background_colour with
    | DefaultColour, DefaultColour ->
        System.Console.Write value
    | _, DefaultColour ->
        let foregroundColour = System.Console.ForegroundColor in
        System.Console.ForegroundColor <- basic_to_system foreground_colour;
        System.Console.Write value;
        System.Console.ForegroundColor <- foregroundColour
    | DefaultColour, _ ->
        let backgroundColour = System.Console.BackgroundColor in
        System.Console.BackgroundColor <- basic_to_system background_colour;
        System.Console.Write value;
        System.Console.BackgroundColor <- backgroundColour
    | _ ->
        let foregroundColour = System.Console.ForegroundColor in
        let backgroundColour = System.Console.BackgroundColor in
        System.Console.ForegroundColor <- basic_to_system foreground_colour;
        System.Console.BackgroundColor <- basic_to_system background_colour;
        System.Console.Write value;
        System.Console.ForegroundColor <- foregroundColour;
        System.Console.BackgroundColor <- backgroundColour

let drawConsoleChar (consoleChar : consoleChar) = 
    match consoleChar with
    | CNil -> ()
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | CDigit (Digit d) -> Console.Write d
    | ColouredString(c, foreground_colour, background_colour) -> consoleWriteColor foreground_colour background_colour c
    | ColouredDigit ((Digit d), foreground_colour, background_colour) -> consoleWriteColor foreground_colour background_colour d
    | NL -> Console.WriteLine ""

let drawConsoleString (consoleString : consoleString) : unit =
    List.iter drawConsoleChar consoleString

let maximise_console() : unit =
    let p = Process.GetCurrentProcess() in
    ShowWindow(p.MainWindowHandle, 3) (* SW_MAXIMIZE = 3 *)
    |> ignore
