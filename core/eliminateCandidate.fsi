module core.eliminateCandidate

open sudoku
open puzzlemap
open hints

val eliminateCandidateHintDescription : PuzzleMap -> Candidate -> HintDescription2

val eliminateCandidateStep : PuzzleMap -> Candidate -> Solution -> Solution
