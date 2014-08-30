module console.format

open core.puzzlemap
open core.sudoku

type gridCharsRow<'a> = 
    { l : 'a
      m : 'a
      r : 'a }

type gridChars<'a> = 
    { h : 'a
      v : gridCharsRow<'a>
      t : gridCharsRow<'a>
      m : gridCharsRow<'a>
      b : gridCharsRow<'a> }

type solutionCharsRow<'a> = 
    { mi : 'a
      x : gridCharsRow<'a> }

type solutionChars<'a> = 
    { h : 'a
      hi : 'a
      v : gridCharsRow<'a>
      vi : 'a
      t : solutionCharsRow<'a>
      m : solutionCharsRow<'a>
      mi : solutionCharsRow<'a>
      b : solutionCharsRow<'a> }

val printRowOnOneLine : ('b -> 'c) -> ('a -> 'b list) -> seq<'c> -> 'a list -> seq<'c>
val printGrid : gridChars<seq<'c>> -> seq<'c> -> (Cell -> 'c) -> PuzzleMaps -> seq<'c>
val print_full : solutionChars<seq<'c>>
     -> seq<'c> -> (Cell -> 'b) -> PuzzleMaps -> Candidate list -> (Candidate -> 'b -> 'c) -> seq<'c>
