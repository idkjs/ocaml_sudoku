open Sudoku
open Puzzlemap
(*F# open FSharp.Compatibility.OCaml F#*)

type basic_color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DarkRed
  | DarkGreen
  | DarkYellow
  | DarkBlue

(* Things we may want to write *)
type consoleChar = 
    | CNil
    | CChar of char
    | CStr of string
    | ColouredChar of char * basic_color
    | ColouredString of string * basic_color
    | NL

type consoleString = consoleChar list

(* Printing a row, we need special characters at left, in the middle and on the right *)
type gridCharsRow = 
    { l : consoleString;
      m : consoleString;
      r : consoleString }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
type gridChars = 
    { h : consoleString;
      v : gridCharsRow;
      t : gridCharsRow;
      m : gridCharsRow;
      b : gridCharsRow;
      n : consoleString }

type candidateGridCharsRow = 
    { mi : consoleString;
      x : gridCharsRow }

type candidateGridChars = 
    { h : consoleString;
      hi : consoleString;
      v : gridCharsRow;
      vi : consoleString;
      t : candidateGridCharsRow;
      m : candidateGridCharsRow;
      mi : candidateGridCharsRow;
      b : candidateGridCharsRow;
      n : consoleString }

val printLine : cells -> (cell -> consoleString) -> consoleString

val printGrid : puzzleMap -> gridChars -> (cell -> consoleString) -> consoleString

val printCandidateGrid : puzzleMap -> candidateGridChars -> digits -> (cell -> digit -> consoleString) -> consoleString
