module console.format

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

val printGrid : int<size> -> int<width> -> int<height> -> gridChars<seq<'c>> -> seq<'c> -> (Cell -> 'c) -> seq<'c>

val print_full : int<size> -> int<width> -> int<height> -> solutionChars<seq<'c>>
     -> seq<'c> -> (Cell -> 'b) -> Candidate list -> (Candidate -> 'b -> 'c) -> seq<'c>
