module hints.hints

open core.sudoku

type CandidateReduction = 
    { cell : Cell
      candidates : Set<Digit> }

type HintDescription = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      pointers : Set<CandidateReduction> }

val first : Set<'a> -> 'a

val setSubsets : List<'a> -> int -> List<List<'a>>

[<NoEquality; NoComparison>]
type HintDescription2 = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      annotations : Cell -> CellAnnotation }

val mhas : (Cell -> Set<Cell>) -> (House -> Set<Cell>) -> HintDescription -> HintDescription2
