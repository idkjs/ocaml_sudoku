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

val printGrid : Stack list
     -> (Stack -> Column list) -> Band list -> (Band -> Row list) -> gridChars<seq<'c>> -> (Cell -> 'c) -> seq<'c>

val printCandidateGrid : Stack list
     -> (Stack -> Column list)
     -> Band list
     -> (Band -> Row list) -> candidateGridChars<seq<'c>> -> Candidate list -> (Cell -> Candidate -> 'c) -> seq<'c>
