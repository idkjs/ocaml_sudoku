open Sudoku
open Puzzlemap

let nakedSingleCell (p : puzzleMap) (cellCandidates : cellCandidates) (cell : cell) : Hint.description option =
    let candidates = CellCandidates.get cellCandidates cell in

    if Digits.count candidates = 1 then 
        let candidate = Digits.first candidates in

        let setCellValue = Value.make cell candidate in

        Some { primaryHouses = Houses.empty;
                secondaryHouses = Houses.empty;
                candidateReductions = [];
                setCellValueAction = Some setCellValue;
                pointers = [];
                focus = Digits.empty }
    else None

let nakedSingle (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.cells
    |> Cells.toList
    |> List.map (nakedSingleCell p cellCandidates)
    |> Sset.choose Sset.id

let findNaked (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) : Hint.description option = 

    let subsetDigits =
        cellSubset
        |> Cells.map (CellCandidates.get cellCandidates)
        |> Digits.unionManyList
        in

    if Digits.count subsetDigits <= count then
        let candidateReductions =
            primaryHouse
            |> Smap.get House.comparer p.houseCells
            |> Cells.filter (fun cell -> Cells.contains cell cellSubset = false) 
            |> Cells.map (fun cell -> 
                let candidates = CellCandidates.get cellCandidates cell in
                CandidateReduction.make cell (Digits.intersect subsetDigits candidates))
            |> List.filter (fun cr -> Digits.count cr.candidates > 0)
            in

        let pointers =
            cellSubset
            |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
            in

        if List.length candidateReductions > 0 then 
            Some { primaryHouses = Houses.singleton primaryHouse;
                   secondaryHouses = Houses.empty;
                   candidateReductions = candidateReductions;
                   setCellValueAction = None;
                   pointers = pointers;
                   focus = Digits.empty }

        else None
    else None

let nakedNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Hint.description list =
    
    let hht = 
        primaryHouse
        |> Smap.get House.comparer p.houseCells
        |> Cells.filter (fun cell -> 
            let candidates = CellCandidates.get cellCandidates cell in
            Digits.count candidates > 1 && Digits.count candidates <= count) 
        in

    Sset.setSubsets (Cells.toList hht) count
    |> List.map (fun ss -> findNaked count p cellCandidates primaryHouse (Cells.ofList ss))
    |> Sset.choose Sset.id

let nakedN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.houses
    |> List.map (nakedNPerHouse i p cellCandidates )
    |> List.concat

let find (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    if i = 1 then nakedSingle p cellCandidates
    else nakedN i p cellCandidates
