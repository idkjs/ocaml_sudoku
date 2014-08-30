module hints.nakedPair

open core.puzzlemap
open core.sudoku
open hints

type NakedPair = 
    { cell1 : Cell
      cell2 : Cell
      candidates : Set<Candidate>
      candidateReductions : Set<CandidateReduction>
      house : House }

val nakedPairFind : (Cell -> Set<Candidate>) -> PuzzleMaps -> NakedPair list

val nakedPairToDescription : NakedPair -> PuzzleMaps -> HintDescription

