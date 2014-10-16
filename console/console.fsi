module console.console

open System

open core.sudoku

open format

// Things we may want to write
type ConsoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

val defaultGridChars : gridChars<seq<ConsoleChar>>

val defaultCandidateGridChars : candidateGridChars<seq<ConsoleChar>>

val drawConsoleChar : ConsoleChar -> Unit

val drawDigitCellContents : Digit option -> CellContents -> ConsoleChar

val drawDigitCellContentAnnotations : Candidate
     -> Candidate -> Digit option -> CellContents -> CellAnnotation option -> ConsoleChar
