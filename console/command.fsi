module console.command

open core.sudoku
open core.puzzlemap
open core.hints

val parseValue : digit array -> string -> digit option

val focusCommandParse : puzzleShape -> string -> digit option

val focusCommandHintDescription : puzzleMap -> digit -> hintDescription

val setCellCommandParse : puzzleShape -> string -> puzzleMap -> value option

val setCellCommandCheck : given -> cellCandidates -> value -> value option

val candidateClearCommandParse : puzzleShape -> string -> puzzleMap -> candidate option

val candidateClearCommandCheck : given -> cellCandidates -> candidate -> candidate option

val SupportedHints : lookup<string, (puzzleMap -> cellCandidates -> hintDescription array) option>
