open Sudoku
open Format

val drawDigitCellString : digit option -> cellContents -> consoleString

val drawDigitCellContentAnnotationString : digit -> (cell * Hint.annotation) list -> cell -> digit -> consoleString
