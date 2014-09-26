module hints.hints

open core.sudoku

type HintDescription = 
    { primaryHouses : Set<House>
      primaryHouseCells : Set<Cell>
      secondaryHouses : Set<House>
      secondaryHouseCells : Set<Cell>
      candidateReductions : Set<CandidateReduction>
      setCellValue : SetCellValue option
      pointers : Set<CandidateReduction> }

val first : Set<'a> -> 'a

val setSubsets : List<'a> -> int -> List<List<'a>>

val mhas : HintDescription -> (Cell -> Set<Candidate>) -> (Cell -> CellAnnotation)
