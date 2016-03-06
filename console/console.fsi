module console.console

open System

open core.sudoku
open core.hints

open format

(* Things we may want to write *)
type consoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

val defaultGridChars : gridChars<seq<consoleChar>>

val defaultCandidateGridChars : candidateGridChars<seq<consoleChar>>

val drawConsoleChar : consoleChar -> Unit

val drawDigitCellContents : digit option -> cellContents -> consoleChar

val drawDigitCellContentAnnotations : digit -> lookup<cell, annotation> -> cell -> digit -> consoleChar
