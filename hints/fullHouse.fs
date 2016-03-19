module hints.fullHouse

(* Full House means:
 For a house there is only one cell that is neither given nor set i.e. has candidates *)

open core.sudoku
open core.puzzlemap
open core.hints

let fullHousePerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription option =

    let hhs =
        p.houseCellCandidateReductions primaryHouse cellCandidates
        |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0) 

    if CandidateReductions.count hhs = 1 then 
        let h = hhs.data.data.Head
        let cell = h.cell
        let candidate = first h.candidates

        let setCellValue = makeValue cell candidate

        Some { hintDescription.primaryHouses = Houses.singleton primaryHouse
               secondaryHouses = Houses.empty
               candidateReductions = CandidateReductions.empty
               setCellValueAction = Some setCellValue
               pointers = CandidateReductions.empty
               focus = Digits.empty }
    else None

let fullHouses (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    p.houses
    |> List.choose (fullHousePerHouse p cellCandidates)
