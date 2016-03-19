open Sudoku
open Puzzlemap
open hints

val eliminateCandidateHintDescription : puzzleMap -> candidate -> hintDescription

val eliminateCandidateStep : puzzleMap -> candidate -> solution -> solution
