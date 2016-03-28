open Smap
open Sudoku
open Puzzlemap

type hintDescription = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReduction list;
      setCellValueAction : value option;
      pointers : candidateReduction list;
      focus : digits }

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
    { annotations : (cell * annotation) list }

val mhas : solution -> puzzleMap -> hintDescription -> hintDescription2

val mhas2 : solution -> puzzleMap -> hintDescription2
