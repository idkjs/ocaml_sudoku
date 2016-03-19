open Sset
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
        |> Digits.unionMany
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
            |> Cells.map (fun cell -> makeCandidateReduction cell (Digits.singleton candidate))
            |> CandidateReductions.ofSet
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
                    |> Cells.map (fun cell -> makeCandidateReduction cell (Digits.singleton candidate))
                    |> CandidateReductions.ofSet
                    in

                if CandidateReductions.count candidateReductions > 0 then 
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
        |> SSet.toList
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
        SMap.ofLookup<cell, house list> p.cells cellLines
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription list = 
    let cellBox (cell : cell) =
        [ SMap.get p.cellBox cell |> HBox ]
        in

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house list> p.cells cellBox
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
        |> List.map HRow
        |> List.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> List.map HColumn
        |> List.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
