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

// To draw a cell we may want to display extra information...
type CellAnnotation = 
    { setValue : Digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : Digit option
      reductions : Set<Digit>
      pointers : Set<Digit> }

type CellAnnotations = Map<Cell, CellAnnotation>

type HintDescription2 = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      annotations : CellAnnotations }

val mhas : Set<Cell> -> CellHouseCells -> HouseCells -> HintDescription -> HintDescription2
