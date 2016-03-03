module core.hints

open sset
open sudoku
open puzzlemap

type hintDescription = 
    { primaryHouses : houses
      secondaryHouses : houses
      candidateReductions : candidateReductions
      setCellValueAction : value option
      pointers : candidateReductions
      focus : digits }

val first : digits -> digit

val setSubsets : 'a array -> int -> 'a array array

(* To draw a cell we may want to display extra information... *)
type annotation = 
    { given : digit option
      setValue : digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : digit option
      reductions : digits
      pointers : digits
      focus : digits }

[<NoComparisonAttribute>]
type hintDescription2 = 
    { primaryHouses : houses
      secondaryHouses : houses
      candidateReductions : candidateReductions
      setCellValueAction : value option
      annotations : lookup<cell, annotation> }

val mhas : solution -> puzzleMap -> hintDescription -> hintDescription2
