module hints.hiddenTriple

open core.sudoku
open hints

type HiddenTriple = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }

val hiddenTripleFind : Candidate list
     -> (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> HiddenTriple list

val hiddenTripleToDescription : HiddenTriple -> HintDescription

