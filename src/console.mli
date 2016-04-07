open Sudoku
open Format

val defaultGridChars : gridChars

val defaultCandidateGridChars : candidateGridChars

val drawDigitCellContents : digit option -> cellContents -> consoleChar

val drawDigitCellString : digit option -> cellContents -> consoleString

val drawDigitCellContentAnnotations : digit -> (cell * Hint.annotation) list -> cell -> digit -> consoleChar

val drawDigitCellContentAnnotationString : digit -> (cell * Hint.annotation) list -> cell -> digit -> consoleString
