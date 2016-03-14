module hints.intersection

open core.sset
open core.smap
open core.sudoku
open core.puzzlemap
open core.hints

type cellHouses = SMap<cell, house array>

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : hintDescription array = 

    let primaryHouseCandidates = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany

    let uniqueSecondaryForCandidate (candidate : digit) : hintDescription array = 
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

        let hintsPerSecondaryHouse (secondaryHouses : house array) : hintDescription option = 
            if Cells.count pointerCells > 1 && Array.length secondaryHouses = 1 then 
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
        |> SSet.toArray
    
    primaryHouseCandidates
    |> Digits.toArray
    |> Array.map uniqueSecondaryForCandidate
    |> Array.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription array =
    let cellLines (cell : cell) =
        [| HRow cell.row; HColumn cell.col |]

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house array> p.cells cellLines

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription array = 
    let cellBox (cell : cell) =
        [| SMap.get p.cellBox cell |> HBox |]

    let secondaryHouseLookups =
        SMap.ofLookup<cell, house array> p.cells cellBox

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =
    p.boxes
    |> Array.map HBox
    |> Array.map (pointingPairsPerBox p cellCandidates) 
    |> Array.concat

let boxLineReductions (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription array =
    let rowHints =
        p.rows
        |> Array.map HRow
        |> Array.map (boxLineReductionsPerHouse p cellCandidates)
        |> Array.concat

    let colHints =
        p.columns
        |> Array.map HColumn
        |> Array.map (boxLineReductionsPerHouse p cellCandidates)
        |> Array.concat

    [| rowHints; colHints |]
    |> Array.concat
