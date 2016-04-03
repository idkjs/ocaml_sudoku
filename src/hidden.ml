open Sudoku
open Puzzlemap

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) : Hint.description option = 

    let pointers = 
        primaryHouse
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
        |> List.map (fun cr -> CandidateReduction.make cr.cell (Digits.intersect cr.candidates candidateSubset))
        |> List.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    let candidateReductions = 
        primaryHouse
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
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
                let h = List.head pointers in
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
        house
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (CellCandidates.get cellCandidates)
        |> Digits.unionManyList
        in

    Sset.setSubsets (Digits.toList houseCandidates) count
    |> List.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.ofList candidateSubset) house)

let find (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.houses
    |> List.map (hiddenNPerHouse i p cellCandidates)
    |> List.concat
