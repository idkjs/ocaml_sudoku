module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open System

open console

open core.setCell
open core.sudoku
open hints

type FullHouse = 
    { setCellValue : SetCellValue
      house : House }
    override this.ToString() = String.Format("fh {0}, {1}", this.house, this.setCellValue)

let fullHousePerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Count = 1 then 
        let h = first hhs

        [ { FullHouse.setCellValue = 
                { SetCellValue.cell = snd h
                  candidate = first (fst h) }
            house = house } ]
    else []

let fullHouseFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (fullHousePerHouse candidateLookup houseCells) houses

let fullHouseToDescription (hint : FullHouse) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = set []
      setCellValue = Some hint.setCellValue
      pointerCells = set []
      pointerCandidates = set [] }
