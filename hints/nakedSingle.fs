module hints.nakedSingle

// Naked Single means:
// For a cell there is only one candidate

open System

open console

open core.setCell
open core.sudoku
open hints

type NakedSingle = 
    { setCellValue : SetCellValue }
    override this.ToString() = String.Format("ns {0}", this.setCellValue)

let nakedSingleFind (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 

    let candidateCells = List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells

    List.map (fun (candidates, cell) -> 
        { NakedSingle.setCellValue = 
              { SetCellValue.cell = cell
                candidate = first candidates } }) filteredCandidateCells

let nakedSingleToDescription (hint : NakedSingle) : HintDescription = 
    { HintDescription.house = None
      candidateReductions = set []
      setCellValue = Some hint.setCellValue
      pointerCells = set []
      pointerCandidates = set [] }
