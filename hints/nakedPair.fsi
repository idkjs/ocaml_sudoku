module hints.nakedPair

open core.sudoku
open hints

type NakedPair = 
    { cell1 : Cell
      cell2 : Cell
      candidates : Set<Candidate>
      candidateReductions : Set<CandidateReduction>
      house : House }

val nakedPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> NakedPair list

val nakedPairToDescription : NakedPair -> HintDescription

