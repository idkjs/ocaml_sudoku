open Sudoku
open Puzzlemap
open Hint
(*F# open FSharp.Compatibility.OCaml F#*)

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) : Hint.description option = 

    let pointers = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse 
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        |> List.map (fun cr -> CandidateReduction.make cr.cell (Digits.intersect cr.candidates candidateSubset))
        |> List.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    let candidateReductions = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        |> List.map
            (fun cr -> 
                let pointerCandidates = Digits.intersect cr.candidates candidateSubset in
            
                let crs = 
                    if Digits.count pointerCandidates > 0 then Digits.difference cr.candidates candidateSubset
                    else Digits.empty
                    in

                let candidateReduction = CandidateReduction.make cr.cell crs in
            
                candidateReduction)
        |> List.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    if List.length pointers = count && List.length candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = List.hd pointers in
                let cell = h.cell in
                let candidate = Digits.first candidateSubset in

                let setCellValue = Value.make cell candidate in

                Some setCellValue
            else None
            in

        Some { primaryHouses = Houses.singleton primaryHouse;
               secondaryHouses = Houses.empty;
               candidateReductions = candidateReductions;
               setCellValueAction = setCellValue;
               pointers = pointers;
               focus = Digits.empty }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (house : house) : Hint.description list = 

    let houseCandidates =
        p.houseCells
        |> Smap.get House.comparer house
        |> Cells.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> Digits.union_many
        in

    Sset.setSubsets (Digits.to_list houseCandidates) count
    |> Sset.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.make candidateSubset) house)

let find (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.houses
    |> Houses.map (hiddenNPerHouse i p cellCandidates)
    |> List.concat
