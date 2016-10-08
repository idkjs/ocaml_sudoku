open Sudoku
open Puzzlemap
open Hint

type cellHouses = (cell * house list) list

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : Hint.description list = 

    let primaryHouseCandidates = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse
        |> Cells.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> Digits.union_many
        in

    let uniqueSecondaryForCandidate (candidate : digit) : Hint.description list = 
        let pointerCells = 
            p.houseCells
            |> Smap.get House.comparer primaryHouse
            |> Cells.filter (fun cell -> 
                let candidates = CellCandidates.get cell cellCandidates in
                Digits.contains candidate candidates) 
            in

        let pointers  = 
            pointerCells
            |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : house list) : Hint.description option = 
            if Cells.count pointerCells > 1 && List.length secondaryHouses = 1 then 
                let primaryHouseCells =
                    p.houseCells
                    |> Smap.get House.comparer primaryHouse
                    in

                let secondaryHouse = List.nth secondaryHouses 0 in
                let secondaryHouseCells = Smap.get House.comparer secondaryHouse p.houseCells in

                let otherHouseCells = Cells.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions = 
                    otherHouseCells
                    |> Cells.filter (fun cell -> 
                        let candidates = CellCandidates.get cell cellCandidates in
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
                            Smap.get Cell.comparer cell secondaryHouseLookups
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> Digits.map uniqueSecondaryForCandidate
    |> List.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Hint.description list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]
        in

    let secondaryHouseLookups =
        Cells.ofLookup cellLines p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Hint.description list = 
    let cellBox (cell : cell) =
        [ Smap.get Cell.comparer cell p.cellBox |> House.make_box ]
        in

    let secondaryHouseLookups =
        Cells.ofLookup cellBox p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    p.boxes
    |> List.map House.make_box
    |> List.map (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    let rowHints =
        p.rows
        |> Rows.map House.make_row
        |> Houses.make
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> Columns.map House.make_column
        |> Houses.make
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
