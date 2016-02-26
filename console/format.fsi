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

type candidateGridCharsRow<'a> = 
    { mi : 'a
      x : gridCharsRow<'a> }

type candidateGridChars<'a> = 
    { h : 'a
      hi : 'a
      v : gridCharsRow<'a>
      vi : 'a
      t : candidateGridCharsRow<'a>
      m : candidateGridCharsRow<'a>
      mi : candidateGridCharsRow<'a>
      b : candidateGridCharsRow<'a>
      n : 'a }

val printLine : cell list -> (cell -> 'c) -> List<'c>

val printGrid : stack list
     -> (stack -> column list) -> band list -> (band -> row list) -> gridChars<seq<'c>> -> (cell -> 'c) -> seq<'c>

val printCandidateGrid : stack list
     -> (stack -> column list)
     -> band list
     -> (band -> row list) -> candidateGridChars<seq<'c>> -> digit list -> (cell -> digit -> 'c) -> seq<'c>
