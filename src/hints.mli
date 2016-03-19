module hints

open Sset
open Smap
open Sudoku
open Puzzlemap

type hintDescription = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReductions;
      setCellValueAction : value option;
      pointers : candidateReductions;
      focus : digits }

val first : digits -> digit

val setSubsets : 'a list -> int -> SSet<'a> list

(* To draw a cell we may want to display extra information... *)
type annotation = 
    { given : digit option;
      current: cellContents;
      setValue : digit option;
      primaryHintHouse : bool;
      secondaryHintHouse : bool;
      setValueReduction : digit option;
      reductions : digits;
      pointers : digits;
      focus : digits }

[<NoComparisonAttribute>]
type hintDescription2 = 
    { annotations : SMap<cell, annotation> }

val mhas : solution -> puzzleMap -> hintDescription -> hintDescription2

val mhas2 : solution -> puzzleMap -> hintDescription2
