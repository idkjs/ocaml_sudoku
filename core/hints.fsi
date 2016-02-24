module core.hints

open sudoku
open puzzlemap

type CandidateReduction = 
    { cell : Cell
      candidates : Set<Digit> }

type HintDescription = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      pointers : Set<CandidateReduction>
      focus : Set<Digit> }

val first : Set<'a> -> 'a

val setSubsets : List<'a> -> int -> List<List<'a>>

// To draw a cell we may want to display extra information...
type Annotation = 
    { given : Digit option
      setValue : Digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : Digit option
      reductions : Set<Digit>
      pointers : Set<Digit>
      focus : Set<Digit> }

type CellAnnotations = Map<Cell, Annotation>

type HintDescription2 = 
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValueAction : Value option
      annotations : CellAnnotations }

val mhas : Solution -> PuzzleMap -> HintDescription -> HintDescription2
