module hints.intersection

open core.sudoku
open core.puzzlemap
open core.hints

let intersectionsPerHouse (p : PuzzleMap) (cellCandidates : CellCandidates) (primaryHouse : House) (secondaryHouseLookups : Map<Cell, Set<House>>) : Set<HintDescription> = 

    let primaryHouseCells =
        primaryHouse
        |> p.houseCells.Get
    
    let primaryHouseCandidates : Set<Digit> = 
        primaryHouseCells
        |> Set.map cellCandidates.Get
        |> Set.unionMany
    
    let uniqueSecondaryForCandidate (candidate : Digit) : Set<HintDescription> = 
        let pointerCells = 
            primaryHouseCells
            |> Set.filter (fun cell -> 
                let candidates = cellCandidates.Get cell
                Set.contains candidate candidates) 
        
        let pointers : Set<CandidateReduction> = 
            pointerCells
            |> Set.map (fun cell -> 
                { CandidateReduction.cell = cell
                  candidates = set [ candidate ] }) 
        
        let hintsPerSecondaryHouse (secondaryHouses : Set<House>) : HintDescription option = 
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
                        { CandidateReduction.cell = cell
                          candidates = set [ candidate ] }) 
                
                if Set.count candidateReductions > 0 then 
                    Some { HintDescription.primaryHouses = set [ primaryHouse ]
                           secondaryHouses = set [ secondaryHouse ]
                           candidateReductions = candidateReductions
                           setCellValueAction = None
                           pointers = pointers
                           focus = set [] }
                else None
            else None
        
        pointerCells
        |> Set.map (fun cell -> 
                        let secondaryHouses : Set<House> = secondaryHouseLookups.Item cell
                        hintsPerSecondaryHouse secondaryHouses)
        |> Set.filter Option.isSome
        |> Set.map Option.get
    
    primaryHouseCandidates
    |> Set.map uniqueSecondaryForCandidate
    |> Set.unionMany

let pointingPairsPerBox (p : PuzzleMap) (cellCandidates : CellCandidates) (primaryHouse : House) : Set<HintDescription> =
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [HRow cell.row; HColumn cell.col ]
            |> Set.ofList
        p.cells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : PuzzleMap) (cellCandidates : CellCandidates) (primaryHouse : House) : Set<HintDescription> = 
    let secondaryHouseLookups : Map<Cell, Set<House>> =
        let ch (cell : Cell) : Set<House> =
            [ p.cellBox.Get cell |> HBox ]
            |> Set.ofList
        p.cells
        |> Set.map (fun cell -> (cell, ch cell))
        |> Map.ofSeq

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : PuzzleMap) (candidateLookup : CellCandidates) : Set<HintDescription> =
    p.boxes
    |> Set.map HBox
    |> Set.map (pointingPairsPerBox p candidateLookup) 
    |> Set.unionMany

let boxLineReductions (p : PuzzleMap) (candidateLookup : CellCandidates) : Set<HintDescription> =
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
