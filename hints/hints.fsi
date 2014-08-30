module hints.hints

open core.puzzlemap
open core.sudoku

type HintDescription = 
    { house : House option
      candidateReductions : Set<CandidateReduction>
      setCellValue : SetCellValue option
      pointerCells : Set<Cell>
      pointerCandidates : Set<Candidate> }

val first : Set<'a> -> 'a

val mhas : HintDescription
     -> PuzzleMaps
     -> (Cell -> Set<Candidate>) -> (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> (Cell -> HintAnnotatedSymbol)
