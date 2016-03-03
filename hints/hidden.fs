module hints.hidden

open core.sudoku
open core.puzzlemap
open core.hints

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) = 

    let pairs = 
        primaryHouse
        |> p.houseCells.Get
        |> Cells.toArray
        |> Array.map (fun cell -> 
            let candidates = cellCandidates.Get cell

            let pointer = 
                { candidateReduction.cell = cell
                  candidates = Digits.intersect candidates candidateSubset }
            
            let crs = 
                if Digits.count pointer.candidates > 0 then Digits.difference candidates candidateSubset
                else Digits.empty
            
            let candidateReduction = 
                { candidateReduction.cell = cell
                  candidates = crs }
            
            (pointer, candidateReduction))

    let pointers, candidateReductions = Array.unzip pairs

    let nonEmptyPointers =
        pointers
        |> Array.filter (fun cr -> Digits.count cr.candidates > 0) 
        |> CandidateReductions.ofArray

    let nonEmptyCandidateReductions =
        candidateReductions
        |> Array.filter (fun cr -> Digits.count cr.candidates > 0) 
        |> CandidateReductions.ofArray

    if CandidateReductions.count nonEmptyPointers = count && CandidateReductions.count nonEmptyCandidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = nonEmptyPointers.data.data.Head
                let cell = h.cell
                let candidate = candidateSubset.data.data.Head

                let setCellValue = makeSetCellDigit cell candidate

                Some setCellValue
            else None

        Some { hintDescription.primaryHouses = Houses.singleton primaryHouse
               secondaryHouses = Houses.empty
               candidateReductions = nonEmptyCandidateReductions
               setCellValueAction = setCellValue
               pointers = nonEmptyPointers
               focus = Digits.empty }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (house : house) : hintDescription array = 

    let houseCandidates =
        house
        |> p.houseCells.Get
        |> Cells.map cellCandidates.Get
        |> Digits.unionMany
        |> Digits.toArray

    setSubsets houseCandidates count
    |> Array.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.ofArray candidateSubset) house)

let hiddenN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =
    p.houses
    |> Array.map (hiddenNPerHouse i p cellCandidates)
    |> Array.concat
