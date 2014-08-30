module hints.fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
open System

open console

open core.puzzlemap
open core.setCell
open core.sudoku
open hints

type FullHouse = 
    { setCellValue : SetCellValue
      house : House }
    override this.ToString() = String.Format("fh {0}, {1}", this.house, this.setCellValue)

let fullHousePerHouse (candidateLookup : Cell -> Set<Candidate>) (puzzleMaps : PuzzleMaps) (house : House) = 

    let cells = getHouseCells puzzleMaps house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hhs = Set.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Count = 1 then 
        let h = first hhs

        [ { FullHouse.setCellValue = 
                { SetCellValue.cell = snd h
                  candidate = first (fst h) }
            house = house } ]
    else []

let fullHouseFind (candidateLookup : Cell -> Set<Candidate>) (puzzleMaps : PuzzleMaps) = 

    let perHouse = fullHousePerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let fullHouseToDescription (hint : FullHouse) (puzzleMaps : PuzzleMaps) (candidateLookup : Cell -> Set<Candidate>) : HintDescription = 

    { HintDescription.house = Some hint.house
      candidateReductions = set []
      setCellValue = Some hint.setCellValue
      pointerCells = set []
      pointerCandidates = set [] }
