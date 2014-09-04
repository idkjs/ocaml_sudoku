module hints.nakedQuad

open core.sudoku
open hints

type NakedQuad = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }

val nakedQuadFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> NakedQuad list

val nakedQuadToDescription : NakedQuad -> HintDescription
