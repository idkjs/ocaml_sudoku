module hints.nakedPair

open core.sudoku
open hints

type NakedPair = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }

val nakedPairFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> NakedPair list

val nakedPairToDescription : NakedPair -> HintDescription

