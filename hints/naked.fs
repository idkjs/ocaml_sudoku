module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

let findNaked (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) = 

    let subsetDigits =
        cellSubset
        |> Cells.map cellCandidates.Get
        |> Digits.unionMany

    if Digits.count subsetDigits <= count then
        let candidateReductions =
            primaryHouse
            |> p.houseCells.Get
            |> Cells.filter (fun cell -> Cells.contains cell cellSubset = false) 
            |> Cells.map (fun cell -> 
                let candidates = cellCandidates.Get cell
                { candidateReduction.candidates = Digits.intersect subsetDigits candidates
                  cell = cell })
            |> CandidateReductions.ofSet
            |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0)

        let pointers =
            cellSubset
            |> Cells.map (fun cell -> { candidateReduction.cell = cell; candidates = cellCandidates.Get cell })
            |> CandidateReductions.ofSet

        if CandidateReductions.count candidateReductions > 0 then 
            Some { hintDescription.primaryHouses = Houses.singleton primaryHouse
                   secondaryHouses = Houses.empty
                   candidateReductions = candidateReductions
                   setCellValueAction = None
                   pointers = pointers
                   focus = Digits.empty }

        else None
    else None

let nakedNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates)  (primaryHouse : house) : hintDescription array =
    
    let hht = 
        primaryHouse
        |> p.houseCells.Get
        |> Cells.filter (fun cell -> 
            let candidates = cellCandidates.Get cell
            Digits.count candidates > 1 && Digits.count candidates <= count) 

    setSubsets (Cells.toArray hht) count
    |> Array.map (fun ss -> findNaked count p cellCandidates primaryHouse (Cells.ofSet ss))
    |> Array.choose id

let nakedSingle (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =

    p.cells
    |> Array.map (fun cell -> 
        let candidates = cellCandidates.Get cell

        if Digits.count candidates = 1 then 
            let candidate = first candidates

            let setCellValue = makeSetCellDigit cell candidate

            Some { hintDescription.primaryHouses = Houses.empty
                   secondaryHouses = Houses.empty
                   candidateReductions = CandidateReductions.empty
                   setCellValueAction = Some setCellValue
                   pointers = CandidateReductions.empty
                   focus = Digits.empty }
        else None)
    |> Array.choose id

let nakedN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =
    p.houses
    |> Array.map (nakedNPerHouse i p cellCandidates )
    |> Array.concat
