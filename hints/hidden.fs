module hints.hidden

open core.sset
open core.smap
open core.sudoku
open core.puzzlemap
open core.hints

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) = 

    let pointers = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        |> CandidateReductions.map (fun cr -> makeCandidateReduction cr.cell (Digits.intersect cr.candidates candidateSubset))
        |> CandidateReductions.ofSet
        |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0) 

    let candidateReductions = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        |> CandidateReductions.map (fun cr -> 
            let pointerCandidates = Digits.intersect cr.candidates candidateSubset
            
            let crs = 
                if Digits.count pointerCandidates > 0 then Digits.difference cr.candidates candidateSubset
                else Digits.empty
            
            let candidateReduction = makeCandidateReduction cr.cell crs
            
            candidateReduction)
        |> CandidateReductions.ofSet
        |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0) 

    if CandidateReductions.count pointers = count && CandidateReductions.count candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = pointers.data.data.Head
                let cell = h.cell
                let candidate = candidateSubset.data.data.Head

                let setCellValue = makeValue cell candidate

                Some setCellValue
            else None

        Some { hintDescription.primaryHouses = Houses.singleton primaryHouse
               secondaryHouses = Houses.empty
               candidateReductions = candidateReductions
               setCellValueAction = setCellValue
               pointers = pointers
               focus = Digits.empty }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (house : house) : hintDescription array = 

    let houseCandidates =
        house
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany

    setSubsets (Digits.toArray houseCandidates) count
    |> Array.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.ofSet candidateSubset) house)

let hiddenN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =
    p.houses
    |> Array.map (hiddenNPerHouse i p cellCandidates)
    |> Array.concat
