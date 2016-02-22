module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

let findNaked (cellHouseCells : CellHouseCells) (candidateLookup : CellCandidates) 
    (primaryHouseCells : Set<Cell>) (cellSubset : Set<Cell>) (count : int) (primaryHouse : House) = 

    let subsetDigits =
        cellSubset
        |> Set.map candidateLookup.Get
        |> Set.unionMany

    if Set.count subsetDigits <= count then 
        let otherCells =
            primaryHouseCells
            |> Set.filter (fun cell -> Set.contains cell cellSubset = false) 

        let candidateReductions =
            otherCells
            |> Set.map (fun cell -> 
                let candidates = candidateLookup.Get cell
                { CandidateReduction.candidates = Set.intersect subsetDigits candidates
                  cell = cell }) 
        
        let nonEmptyCandidateReductions =
            candidateReductions
            |> Set.filter (fun cr -> Set.count cr.candidates > 0)
        
        let pointers =
            cellSubset
            |> Set.map (fun cell -> 
                let candidates = candidateLookup.Get cell
                { CandidateReduction.cell = cell
                  candidates = candidates })

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { HintDescription.primaryHouses = set [ primaryHouse ]
                   secondaryHouses = set []
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValueAction = None
                   pointers = pointers }

        else None
    else None

let nakedNPerHouse (allCells : Set<Cell>) (cellHouseCells : CellHouseCells) (puzzleHouseCells : HouseCells) 
    (candidateLookup : CellCandidates) (count : int) (primaryHouse : House) : Set<HintDescription2> =
    let primaryHouseCells =
        primaryHouse
        |> puzzleHouseCells.Get
    
    let hht = 
        primaryHouseCells
        |> Set.filter (fun cell -> 
            let candidates = candidateLookup.Get cell
            Set.count candidates > 1 && Set.count candidates <= count) 
    
    setSubsets (Set.toList hht) count
    |> Set.ofList
    |> Set.map 
        (fun subset -> 
        findNaked cellHouseCells candidateLookup primaryHouseCells (Set.ofList subset) count primaryHouse)
    |> Set.filter Option.isSome
    |> Set.map Option.get
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)

let nakedSingleFind (allCells : Set<Cell>) (cellHouseCells : CellHouseCells) (puzzleHouseCells : HouseCells) 
    (candidateLookup : CellCandidates) (cells : Set<Cell>) : Set<HintDescription2> = 

    cells
    |> Set.map (fun cell -> 
        let candidates = candidateLookup.Get cell

        if Set.count candidates = 1 then 
            let candidate = first candidates

            let setCellValue = makeSetCellDigit cell candidate

            Some { HintDescription.primaryHouses = set []
                   secondaryHouses = set []
                   candidateReductions = set []
                   setCellValueAction = Some setCellValue
                   pointers = set [] }
        else None)
    |> Set.filter Option.isSome
    |> Set.map Option.get
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)

let nakedSingle (p : PuzzleMap) (candidateLookup : CellCandidates) : Set<HintDescription2> =
    nakedSingleFind p.cells p.cellHouseCells p.houseCells candidateLookup p.cells

let nakedN (i : int) (p : PuzzleMap) (candidateLookup : CellCandidates) : Set<HintDescription2> =
    p.houses
    |> Set.map (nakedNPerHouse p.cells p.cellHouseCells p.houseCells candidateLookup i)
    |> Set.unionMany
