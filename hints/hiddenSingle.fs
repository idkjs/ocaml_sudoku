module hints.hiddenSingle

// Hidden Single means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open System

open console

open core.setCell
open core.sudoku
open hints

type HiddenSingle = 
    { setCellValue : SetCellValue
      house : House }
    override this.ToString() = String.Format("hs {0}, {1}", this.house, this.setCellValue)

let hiddenSinglesPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells
    
    let hs = 
        List.map (fun symbol -> 
            let filteredCandidateCells = 
                Set.filter (fun (candidates, _) -> Set.contains symbol candidates) candidateCells
            let cells = Set.map snd filteredCandidateCells
            (filteredCandidateCells, symbol)) alphabet
    
    let hhs = List.filter (fun (filteredCandidateCells, _) -> Set.count filteredCandidateCells = 1) hs
    
    let hhhs = 
        List.map (fun (filteredCandidateCells, candidate) -> 
            { HiddenSingle.setCellValue = 
                  { SetCellValue.cell = first filteredCandidateCells |> snd
                    candidate = candidate }
              house = house }) hhs

    hhhs

let hiddenSingleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenSinglesPerHouse alphabet candidateLookup houseCells) houses

let hiddenSingleToDescription (hint : HiddenSingle) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = set []
      setCellValue = Some hint.setCellValue
      pointerCells = set []
      pointerCandidates = set [] }
