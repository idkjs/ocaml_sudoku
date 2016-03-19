module command

open Smap
open Sudoku
open Puzzlemap
open Hints

val parseValue : digit list -> string -> digit option

val focusCommandParse : puzzleShape -> string -> digit option

val focusCommandHintDescription : puzzleMap -> digit -> hintDescription

val setCellCommandParse : puzzleShape -> string -> puzzleMap -> value option

val setCellCommandCheck : given -> cellCandidates -> value -> value option

val candidateClearCommandParse : puzzleShape -> string -> puzzleMap -> candidate option

val candidateClearCommandCheck : given -> cellCandidates -> candidate -> candidate option

val supportedHints : SMap<string, (puzzleMap -> cellCandidates -> hintDescription list)>
