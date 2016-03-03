module hints.intersection


open core.sset;open core.sudoku
open core.puzzlemap
open core.hints

type cellHouses = lookup<cell, house array>

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : hintDescription array = 

    let primaryHouseCells =
        primaryHouse
        |> p.houseCells.Get
    
    let primaryHouseCandidates = 
        primaryHouseCells
        |> Cells.toArray
        |> Array.map cellCandidates.Get
        |> Digits.unionMany
        |> Digits.toArray

    let uniqueSecondaryForCandidate (candidate : digit) : hintDescription array = 
        let pointerCells = 
            primaryHouseCells
            |> Cells.filter (fun cell -> 
                let candidates = cellCandidates.Get cell
                Digits.contains candidate candidates) 
        
        let pointers  = 
            pointerCells
            |> Cells.map (fun cell -> 
                { candidateReduction.cell = cell
                  candidates = Digits.singleton candidate }) 
            |> CandidateReductions.ofSet

        let hintsPerSecondaryHouse (secondaryHouses : house array) : hintDescription option = 
            if Cells.count pointerCells > 1 && Array.length secondaryHouses = 1 then 
                let secondaryHouse = secondaryHouses.[0]
                let secondaryHouseCells = p.houseCells.Get secondaryHouse
                let otherHouseCells = Cells.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductions = 
                    otherHouseCells
                    |> Cells.filter (fun cell -> 
                        let candidates = cellCandidates.Get cell
                        Digits.contains candidate candidates) 
                    |> Cells.map (fun cell -> 
                        { candidateReduction.cell = cell
                          candidates = Digits.singleton candidate }) 
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
                            secondaryHouseLookups.Get cell
                            |> hintsPerSecondaryHouse)
        |> SSet.toArray
    
    primaryHouseCandidates
    |> Array.map uniqueSecondaryForCandidate
    |> Array.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription array =
    let cellLines (cell : cell) =
        [| HRow cell.row; HColumn cell.col |]

    let secondaryHouseLookups =
        makeMapLookup<cell, house array> p.cells cellLines
        :> cellHouses

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : hintDescription array = 
    let cellBox (cell : cell) =
        [| p.cellBox.Get cell |> HBox |]

    let secondaryHouseLookups =
        makeMapLookup<cell, house array> p.cells cellBox
        :> cellHouses

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
