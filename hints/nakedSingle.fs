module hints.nakedSingle

// Naked Single means:
// For a cell there is only one candidate

open core.sudoku
open hints

let nakedSingleFind (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 

    let candidateCells = List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells

    List.map (fun (candidates, cell) -> 
        { HintDescription.house = None
          candidateReductions = set []
          setCellValue = Some { SetCellValue.cell = cell
                                candidate = first candidates }
          pointers = set [] }

        ) filteredCandidateCells
