module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : Map<cell, Set<house>>) : Set<hintDescription> = 

    let primaryHouseCells =
        primaryHouse
        |> p.houseCells.Get
    
    let primaryHouseCandidates : Set<digit> = 
        primaryHouseCells
        |> Set.map cellCandidates.Get
        |> Set.unionMany
    
    let uniqueSecondaryForCandidate (candidate : digit) : Set<hintDescription> = 
        let pointerCells = 
            primaryHouseCells
            |> Set.filter (fun cell -> 
                let candidates = cellCandidates.Get cell
                Set.contains candidate candidates) 
        
        let pointers : Set<candidateReduction> = 
            pointerCells
            |> Set.map (fun cell -> 
                { candidateReduction.cell = cell
                  candidates = set [ candidate ] }) 
        
        let hintsPerSecondaryHouse (secondaryHouses : Set<house>) : hintDescription option = 
            if Set.count pointerCells > 1 && Set.count secondaryHouses = 1 then 
                let secondaryHouse = first secondaryHouses
                let secondaryHouseCells = p.houseCells.Get secondaryHouse
                let otherHouseCells = Set.difference secondaryHouseCells primaryHouseCells
                
                let candidateReductionCells =
                    otherHouseCells
                    |> Set.filter (fun cell -> 
                        let candidates = cellCandidates.Get cell
                        Set.contains candidate candidates) 
                
                let candidateReductions = 
                    candidateReductionCells
                    |> Set.map (fun cell -> 
                        { candidateReduction.cell = cell
                          candidates = set [ candidate ] }) 
                
                if Set.count candidateReductions > 0 then 
                    Some { hintDescription.primaryHouses = set [ primaryHouse ]
                           secondaryHouses = set [ secondaryHouse ]
                           candidateReductions = candidateReductions
                           setCellValueAction = None
                           pointers = pointers
                           focus = set [] }
                else None
            else None
        
        pointerCells
        |> Set.map (fun cell -> 
                        let secondaryHouses : Set<house> = secondaryHouseLookups.Item cell
                        hintsPerSecondaryHouse secondaryHouses)
        |> Set.filter Option.isSome
        |> Set.map Option.get
    
    primaryHouseCandidates
    |> Set.map uniqueSecondaryForCandidate
    |> Set.unionMany

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Set<hintDescription> =
    let secondaryHouseLookups : Map<cell, Set<house>> =
        let ch (cell : cell) : Set<house> =
            [HRow cell.row; HColumn cell.col ]
            |> Set.ofList
        p.cells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : Set<hintDescription> = 
    let secondaryHouseLookups : Map<cell, Set<house>> =
        let ch (cell : cell) : Set<house> =
            [ p.cellBox.Get cell |> HBox ]
            |> Set.ofList
        p.cells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    p.boxes
    |> Set.map HBox
    |> Set.map (pointingPairsPerBox p candidateLookup) 
    |> Set.unionMany

let boxLineReductions (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    let rowHints =
        p.rows
        |> Set.map HRow
        |> Set.map (boxLineReductionsPerHouse p candidateLookup)
        |> Set.unionMany

    let colHints =
        p.columns
        |> Set.map HColumn
        |> Set.map (boxLineReductionsPerHouse p candidateLookup)
        |> Set.unionMany

    [rowHints; colHints]
    |> Set.ofList
    |> Set.unionMany
