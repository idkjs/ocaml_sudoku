module hints.hiddenPair

open core.sudoku
open hints

type HiddenPair = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }

val hiddenPairFind : Candidate list -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HiddenPair list

val hiddenPairToDescription : HiddenPair -> HintDescription
