(* Full House means:
 For a house there is only one cell that is neither given nor set i.e. has candidates *)

open Sudoku
open Puzzlemap
open Hints

let fullHousePerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription option =

    let hhs =
        p.houseCellCandidateReductions primaryHouse cellCandidates
        |> List.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    if List.length hhs = 1 then 
        let h = List.head hhs in
        let cell = h.cell in
        let candidate = Digits.first h.candidates in

        let setCellValue = Value.make cell candidate in

        Some { hintDescription.primaryHouses = Houses.singleton primaryHouse;
               secondaryHouses = Houses.empty;
               candidateReductions = [];
               setCellValueAction = Some setCellValue;
               pointers = [];
               focus = Digits.empty }
    else None

let fullHouses (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    p.houses
    |> List.choose (fullHousePerHouse p cellCandidates)
