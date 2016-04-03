open Sudoku
open Puzzlemap

type cellHouses = (cell * house list) list

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : Hint.description list = 

    let primaryHouseCandidates = 
        primaryHouse
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (CellCandidates.get cellCandidates)
        |> Digits.unionManyList
        in

    let uniqueSecondaryForCandidate (candidate : digit) : Hint.description list = 
        let pointerCells = 
            primaryHouse
            |> Smap.get House.comparer p.houseCells
            |> Cells.filter (fun cell -> 
                let candidates = CellCandidates.get cellCandidates cell in
                Digits.contains candidate candidates) 
            in

        let pointers  = 
            pointerCells
            |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : house list) : Hint.description option = 
            if Cells.count pointerCells > 1 && List.length secondaryHouses = 1 then 
                let primaryHouseCells =
                    primaryHouse
                    |> Smap.get House.comparer p.houseCells
                    in

                let secondaryHouse = secondaryHouses.[0] in
                let secondaryHouseCells = Smap.get House.comparer p.houseCells secondaryHouse in

                let otherHouseCells = Cells.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions = 
                    otherHouseCells
                    |> Cells.filter (fun cell -> 
                        let candidates = CellCandidates.get cellCandidates cell in
                        Digits.contains candidate candidates)
                    |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
                    in

                if List.length candidateReductions > 0 then 
                    Some { primaryHouses = Houses.singleton primaryHouse;
                           secondaryHouses = Houses.singleton secondaryHouse;
                           candidateReductions = candidateReductions;
                           setCellValueAction = None;
                           pointers = pointers;
                           focus = Digits.empty }
                else None
            else None
            in

        pointerCells
        |> Cells.choose (fun cell -> 
                            Smap.get Cell.comparer secondaryHouseLookups cell
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> Digits.toList
    |> List.map uniqueSecondaryForCandidate
    |> List.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Hint.description list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]
        in

    let secondaryHouseLookups =
        Smap.ofLookup (Cells.toList p.cells) cellLines
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Hint.description list = 
    let cellBox (cell : cell) =
        [ Smap.get Cell.comparer p.cellBox cell |> HBox ]
        in

    let secondaryHouseLookups =
        Smap.ofLookup (Cells.toList p.cells) cellBox
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.boxes
    |> List.map HBox
    |> List.map (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    let rowHints =
        p.rows
        |> Rows.map HRow
        |> Houses.ofList
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> Columns.map HColumn
        |> Houses.ofList
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
