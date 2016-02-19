module hints.naked

open console

open core.setCell
open core.sudoku
open core.puzzlemap
open hints

let findNaked (cellHouseCells : MapCellHouseCells) (candidateLookup : MapCellCandidates) 
    (primaryHouseCells : Set<Cell>) (cellSubset : Set<Cell>) (count : int) (primaryHouse : House) = 
    let digits = Set.map (fun cell -> candidateLookup.Item cell) cellSubset
    let subsetDigits = Set.unionMany digits

    if Set.count subsetDigits <= count then 
        let otherCells = Set.filter (fun cell -> Set.contains cell cellSubset = false) primaryHouseCells
        
        let candidateReductions = 
            Set.map (fun cell -> 
                let candidates = candidateLookup.Item cell
                { CandidateReduction.candidates = Set.intersect subsetDigits candidates
                  cell = cell }) otherCells
        
        let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.candidates > 0) candidateReductions
        
        let pointers = 
            Set.map (fun cell -> 
                let candidates = candidateLookup.Item cell
                { CandidateReduction.cell = cell
                  candidates = candidates }) cellSubset

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { HintDescription.primaryHouses = set [ primaryHouse ]
                   secondaryHouses = set []
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValueAction = None
                   pointers = pointers }

        else None
    else None

let nakedNPerHouse (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : MapHouseCells) 
    (candidateLookup : MapCellCandidates) (count : int) (primaryHouse : House) : Set<HintDescription2> =
    let primaryHouseCells = puzzleHouseCells.Item primaryHouse
    
    let hht = 
        primaryHouseCells
        |> Set.filter (fun cell -> 
            let candidates = candidateLookup.Item cell
            Set.count candidates > 1 && Set.count candidates <= count) 
    
    let subsets = setSubsets (Set.toList hht) count
    let hs = 
        subsets
        |> Set.ofList
        |> Set.map 
            (fun subset -> 
            findNaked cellHouseCells candidateLookup primaryHouseCells (Set.ofList subset) count primaryHouse)
        |> Set.filter Option.isSome
        |> Set.map Option.get

    hs
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)

let nakedSingleFind (allCells : Set<Cell>) (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : MapHouseCells) 
    (candidateLookup : MapCellCandidates) (cells : Set<Cell>) : Set<HintDescription2> = 
    let hs =
        cells
        |> Set.map (fun cell -> 
            let candidates = candidateLookup.Item cell

            if Set.count candidates = 1 then 
                let candidate = first candidates

                let setCellValue = makeSetCellDigit cell candidate

                Some { HintDescription.primaryHouses = set []
                       secondaryHouses = set []
                       candidateReductions = set []
                       setCellValueAction = Some setCellValue
                       pointers = set [] }
            else None)
    hs
    |> Set.filter Option.isSome
    |> Set.map Option.get
    |> Set.map (mhas allCells cellHouseCells puzzleHouseCells)
