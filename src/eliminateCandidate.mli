module eliminateCandidate

open sudoku
open puzzlemap
open hints

val eliminateCandidateHintDescription : puzzleMap -> candidate -> hintDescription

val eliminateCandidateStep : puzzleMap -> candidate -> solution -> solution
