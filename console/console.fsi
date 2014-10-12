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

val defaultSolutionChars : solutionChars<seq<ConsoleChar>>

val drawConsoleChar : ConsoleChar -> Unit

val drawSymbolCellContents : Symbol option -> CellContents -> ConsoleChar

val drawSymbolCellContentAnnotations : Candidate
     -> Candidate -> Symbol option -> CellContents -> CellAnnotation option -> ConsoleChar
