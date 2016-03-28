module command

open Smap
open Sudoku
open Puzzlemap
open Hints

type parse_column_or_row_results =
    | CROk of int
    | CRError of string * int

val parse_column_or_row_results_to_string : parse_column_or_row_results -> string

type parse_cell_results = 
    | COk of cell
    | CColError of parse_column_or_row_results * int
    | CRowError of int * parse_column_or_row_results
    | CColRowError of parse_column_or_row_results * parse_column_or_row_results

val parse_cell_results_to_string : parse_cell_results -> string

type parse_value_result =
    | VOk of digit
    | VErrorInvalid of string * string
    | VErrorTooMany of string

val parse_value_result_to_string : parse_value_result -> string

val parseValue : digits -> string -> parse_value_result

type focus_command_result =
    | FCOk of parse_value_result
    | FCWrongTermCount of int

val focusCommandParse : puzzleShape -> string -> focus_command_result

val focusCommandHintDescription : puzzleMap -> digit -> hintDescription

type set_cell_command_parse_result =
    | SCCOk of value
    | SCCBadParams of parse_cell_results * parse_value_result
    | SCCWrongTermCount of int

val setCellCommandParse : puzzleShape -> string -> puzzleMap -> set_cell_command_parse_result

type set_cell_command_check_result =
    | SSCROk of value
    | SCCRGiven of value * digit
    | SCCRNotACandidate of value

val set_cell_command_check_result_to_string : set_cell_command_check_result -> string

val setCellCommandCheck : given -> cellCandidates -> value -> set_cell_command_check_result

type clear_candidate_command_parse_result =
    | CCCPROk of candidate
    | CCCPRParseError of parse_cell_results * parse_value_result
    | CCCPRWrongItemCount of int

val candidateClearCommandParse : puzzleShape -> string -> puzzleMap -> clear_candidate_command_parse_result

type clear_candidate_command_check_result =
    | CCCCROk of candidate
    | CCCCRGiven of candidate * digit
    | CCCCRNotACandidate of candidate

val clear_candidate_command_check_result_to_string : clear_candidate_command_check_result -> string

val candidateClearCommandCheck : given -> cellCandidates -> candidate -> clear_candidate_command_check_result

val supportedHints : (string * (puzzleMap -> cellCandidates -> hintDescription list)) list
