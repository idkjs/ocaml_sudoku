open Sset
open Smap
open Sudoku
open Puzzlemap
open Hints

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) = 

    let pointers = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        |> CandidateReductions.map (fun cr -> makeCandidateReduction cr.cell (Digits.intersect cr.candidates candidateSubset))
        |> CandidateReductions.ofSet
        |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    let candidateReductions = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        |> CandidateReductions.map
            (fun cr -> 
                let pointerCandidates = Digits.intersect cr.candidates candidateSubset in
            
                let crs = 
                    if Digits.count pointerCandidates > 0 then Digits.difference cr.candidates candidateSubset
                    else Digits.empty
                    in

                let candidateReduction = makeCandidateReduction cr.cell crs in
            
                candidateReduction)
        |> CandidateReductions.ofSet
        |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    if CandidateReductions.count pointers = count && CandidateReductions.count candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = pointers.data.data.Head in
                let cell = h.cell in
                let candidate = candidateSubset.data.data.Head in

                let setCellValue = makeValue cell candidate in

                Some setCellValue
            else None
            in

        Some { hintDescription.primaryHouses = Houses.singleton primaryHouse;
               secondaryHouses = Houses.empty;
               candidateReductions = candidateReductions;
               setCellValueAction = setCellValue;
               pointers = pointers;
               focus = Digits.empty }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (house : house) : hintDescription list = 

    let houseCandidates =
        house
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany
        in

    setSubsets (Digits.toList houseCandidates) count
    |> List.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.ofSet candidateSubset) house)

let hiddenN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    p.houses
    |> List.map (hiddenNPerHouse i p cellCandidates)
    |> List.concat
