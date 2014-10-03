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
      b : gridCharsRow<'a>
      n : 'a }

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
      b : solutionCharsRow<'a>
      n : 'a }

val printRowOnOneLine : ('b -> 'c) -> ('a -> 'b list) -> seq<'c> -> 'a list -> seq<'c>

val printGrid : (Stack list) -> (Stack -> Column list) -> (Band list) -> (Band -> Row list) -> gridChars<seq<'c>> -> (Cell -> 'c) -> seq<'c>

val print_full : (Stack list) -> (Stack -> Column list) -> (Band list) -> (Band -> Row list) -> solutionChars<seq<'c>> -> Candidate list -> (Cell -> Candidate -> 'c) -> seq<'c>
