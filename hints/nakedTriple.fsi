module hints.nakedTriple

open core.sudoku
open hints

type NakedTriple = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }

val nakedTripleFind : (Cell -> Set<Candidate>) -> (House -> Set<Cell>) -> House list -> NakedTriple list

val nakedTripleToDescription : NakedTriple -> HintDescription


