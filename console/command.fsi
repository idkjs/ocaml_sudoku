module console.command

open core.sudoku
open core.puzzlemap
open core.hints

val parseValue : Digit list -> string -> Digit option

val focusCommandParse : PuzzleShape -> string -> Digit option

val focusCommandHintDescription : PuzzleMap -> Digit -> HintDescription

val setCellCommandParse : PuzzleShape -> string -> PuzzleMap -> Value option

val setCellCommandCheck : Given -> CellCandidates -> Value -> Value option

val candidateClearCommandParse : PuzzleShape -> string -> PuzzleMap -> Candidate option

val candidateClearCommandCheck : Given -> CellCandidates -> Candidate -> Candidate option

val SupportedHints : Map<string, PuzzleMap -> CellCandidates -> Set<HintDescription>>
