module console_win

open Format
open System

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
let consoleWriteColor (value : 'a) consoleColour = 
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
    | ColouredChar(c, basic_color) -> consoleWriteColor c (basic_to_system basic_color)
    | ColouredString(c, basic_color) -> consoleWriteColor c (basic_to_system basic_color)
    | NL -> Console.WriteLine ""

let drawConsoleString (consoleString : consoleString) : Unit =
    List.iter drawConsoleChar consoleString
