module hints.hints

open core.sudoku
open core.puzzlemap

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

type HintDescription2 = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      annotations : Map<Cell, CellAnnotation> }

val mhas : Set<Cell> -> MapCellHouseCells -> (House -> Set<Cell>) -> HintDescription -> HintDescription2
