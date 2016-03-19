module intersection

open sset
open smap
open sudoku
open puzzlemap
open hints

type cellHouses = SMap<cell, house list>

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : hintDescription list = 

    let primaryHouseCandidates = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany

    let uniqueSecondaryForCandidate (candidate : digit) : hintDescription list = 
        let pointerCells = 
            primaryHouse
            |> SMap.get p.houseCells
            |> Cells.filter (fun cell -> 
                let candidates = SMap.get cellCandidates cell
                Digits.contains candidate candidates) 
        
        let pointers  = 
            pointerCells
            |> Cells.map (fun cell -> makeCandidateReduction cell (Digits.singleton candidate))
            |> CandidateReductions.ofSet

        let hintsPerSecondaryHouse (secondaryHouses : house list) : hintDescription option = 
            if Cells.count pointerCells > 1 && List.length secondaryHouses = 1 then 
                let primaryHouseCells =
                    primaryHouse
                    |> SMap.get p.houseCells

                let secondaryHouse = secondaryHouses.[0]
                let secondaryHouseCells = SMap.get p.houseCells secondaryHouse

                let otherHouseCells = Cells.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductions = 
                    otherHouseCells
                    |> Cells.filter (fun cell -> 
                        let candidates = SMap.get cellCandidates cell
                        Digits.contains candidate candidates) 
                    |> Cells.map (fun cell -> makeCandidateReduction cell (Digits.singleton candidate))
                    |> CandidateReductions.ofSet

                if CandidateReductions.count candidateReductions > 0 then 
                    Some { hintDescription.primaryHouses = Houses.singleton primaryHouse
                           secondaryHouses = Houses.singleton secondaryHouse
                           candidateReductions = candidateReductions
                           setCellValueAction = None
                           pointers = pointers
                           focus = Digits.empty }
                else None
            else None
        
        pointerCells
        |> Cells.choose (fun cell -> 
                            SMap.get secondaryHouseLookups cell
                            |> hintsPerSecondaryHouse)
        |> SSet.toList
    
    primaryHouseCandidates
    |> Digits.toList
    |> List.map uniqueSecondaryForCandidate
    |> List.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house list> p.cells cellLines

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription list = 
    let cellBox (cell : cell) =
        [ SMap.get p.cellBox cell |> HBox ]

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house list> p.cells cellBox

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

    let colHints =
        p.columns
        |> List.map HColumn
        |> List.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat

    [| rowHints; colHints |]
    |> List.concat
