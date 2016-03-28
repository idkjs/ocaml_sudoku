open Smap
open Sudoku
open Puzzlemap
open Hints

type cellHouses = SMap<cell, house list>

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : hintDescription list = 

    let primaryHouseCandidates = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionManyList
        in

    let uniqueSecondaryForCandidate (candidate : digit) : hintDescription list = 
        let pointerCells = 
            primaryHouse
            |> SMap.get p.houseCells
            |> Cells.filter (fun cell -> 
                let candidates = SMap.get cellCandidates cell in
                Digits.contains candidate candidates) 
            in

        let pointers  = 
            pointerCells
            |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : house list) : hintDescription option = 
            if Cells.count pointerCells > 1 && List.length secondaryHouses = 1 then 
                let primaryHouseCells =
                    primaryHouse
                    |> SMap.get p.houseCells
                    in

                let secondaryHouse = secondaryHouses.[0] in
                let secondaryHouseCells = SMap.get p.houseCells secondaryHouse in

                let otherHouseCells = Cells.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions = 
                    otherHouseCells
                    |> Cells.filter (fun cell -> 
                        let candidates = SMap.get cellCandidates cell in
                        Digits.contains candidate candidates)
                    |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
                    in

                if List.length candidateReductions > 0 then 
                    Some { hintDescription.primaryHouses = Houses.singleton primaryHouse;
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
                            SMap.get secondaryHouseLookups cell
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> Digits.toList
    |> List.map uniqueSecondaryForCandidate
    |> List.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]
        in

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house list> (Cells.toList p.cells) cellLines
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription list = 
    let cellBox (cell : cell) =
        [ SMap.get p.cellBox cell |> HBox ]
        in

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house list> (Cells.toList p.cells) cellBox
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    p.boxes
    |> List.map HBox
    |> List.map (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
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
