open Sudoku
open Format

(*F# open FSharp.Compatibility.OCaml F#*)

let cs (c : char) : consoleString = [CChar c]

(* Some predefined characters - for smaller grid *)
let defaultGridChars : gridChars = 
    { h = cs '-' (*'─'*);
      v = 
          { l = cs '|' (*'│'*);
            m = cs '|' (*'│'*);
            r = cs '|' (*'│'*) };
      t = 
          { l = cs '*' (*'┌'*);
            m = cs '*' (*'┬'*);
            r = cs '*' (*'┐'*) };
      m = 
          { l = cs '*' (*'├'*);
            m = cs '*' (*'┼'*);
            r = cs '*' (*'┤'*) };
      b = 
          { l = cs '*' (*'└'*);
            m = cs '*' (*'┴'*);
            r = cs '*' (*'┘'*) };
      n = [NL] }

(* Some predefined characters - for smaller grid *)
let defaultCandidateGridChars : candidateGridChars = 
    { h = cs '=' (*'═'*);
      hi = cs '-' (*'─'*);
      v = 
          { l = cs '|' (*'║'*);
            m = cs '|' (*'║'*);
            r = cs '|' (*'║'*) };
      vi = cs '"' (*'│'*);
      t = 
          { mi = cs '#' (*'╦'*);
            x = 
                { l = cs '*' (*'╔'*);
                  m = cs '*' (*'╦'*);
                  r = cs '*' (*'╗'*) } };
      m = 
          { mi = cs '#' (*'╬'*);
            x = 
                { l = cs '*' (*'╠'*);
                  m = cs '*' (*'╬'*);
                  r = cs '*' (*'╣'*) } };
      mi = 
          { mi = cs '#' (*'┼'*);
            x = 
                { l = cs '*' (*'╠'*);
                  m = cs '*' (*'╬'*);
                  r = cs '*' (*'╣'*) } };
      b = 
          { mi = cs '#' (*'╧'*);
            x = 
                { l = cs '*' (*'╚'*);
                  m = cs '*' (*'╩'*);
                  r = cs '*' (*'╝'*) } };
      n = [NL] }

let basic_to_system (color : basic_color) : int =
    match color with
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
    | DarkRed -> 1
    | DarkGreen -> 2
    | DarkYellow -> 3
    | DarkBlue -> 4

(* Change the console colour to write a string *)
let consoleWriteColourChar (consoleColour : int) (value : char) : unit = 
    Printf.printf "\027[38;5;%dm%c\027[0m" consoleColour value

let consoleWriteColourString (consoleColour : int) (value : string) : unit = 
    Printf.printf "\027[38;5;%dm%s\027[0m" consoleColour value

let drawConsoleChar (consoleChar : consoleChar) : unit = 
    match consoleChar with
    | CNil -> ()
    | CChar c -> print_char c
    | CStr c -> print_string c
    | CDigit (Digit d) -> print_char d
    | ColouredChar (c, basic_color) -> consoleWriteColourChar (basic_to_system basic_color) c
    | ColouredString (c, basic_color) -> consoleWriteColourString (basic_to_system basic_color) c
    | ColouredDigit ((Digit d), basic_color) -> consoleWriteColourChar (basic_to_system basic_color) d
    | NL -> print_endline ""

let drawConsoleString (consoleString : consoleString) : unit =
    List.iter drawConsoleChar consoleString

let maximise_console() : unit =
    ()
