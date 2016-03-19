open Sudoku
open Puzzlemap
open Hints

val eliminateCandidateHintDescription : puzzleMap -> candidate -> hintDescription

val eliminateCandidateStep : puzzleMap -> candidate -> solution -> solution
