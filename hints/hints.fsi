module hints.hints

open core.sudoku

type HintDescription = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValue : SetCellValue option
      pointers : Set<CandidateReduction> }

val first : Set<'a> -> 'a

val mhas : HintDescription
     -> (House -> Set<Cell>)
     -> (Cell -> Set<Cell>)
     -> (Cell -> Set<Candidate>) -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> (Cell -> HintAnnotatedSymbol)
