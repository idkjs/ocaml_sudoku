module core.hints

open sudoku
open puzzlemap

type candidateReduction = 
    { cell : cell
      candidates : Set<digit> }

type hintDescription = 
    { primaryHouses : Set<house>
      secondaryHouses : Set<house>
      candidateReductions : Set<candidateReduction>
      setCellValueAction : value option
      pointers : Set<candidateReduction>
      focus : Set<digit> }

val first : Set<'a> -> 'a

val setSubsets : List<'a> -> int -> List<List<'a>>

// To draw a cell we may want to display extra information...
type annotation = 
    { given : digit option
      setValue : digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : digit option
      reductions : Set<digit>
      pointers : Set<digit>
      focus : Set<digit> }

type cellAnnotations = Map<cell, annotation>

type hintDescription2 = 
    { primaryHouses : Set<house>
      secondaryHouses : Set<house>
      candidateReductions : Set<candidateReduction>
      setCellValueAction : value option
      annotations : cellAnnotations }

val mhas : solution -> puzzleMap -> hintDescription -> hintDescription2
