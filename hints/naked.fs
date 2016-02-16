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

let nakedNPerHouse (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : MapCellCandidates) (count : int) (primaryHouse : House) = 
    let primaryHouseCells = puzzleHouseCells primaryHouse
    
    let hht = 
        Set.filter (fun cell -> 
            let candidates = candidateLookup.Item cell
            Set.count candidates > 1 && Set.count candidates <= count) primaryHouseCells
    
    let subsets = setSubsets (Set.toList hht) count
    let hs = 
        List.map 
            (fun subset -> 
            findNaked cellHouseCells candidateLookup primaryHouseCells (Set.ofList subset) count primaryHouse) subsets
    List.choose id hs |> List.map (mhas cellHouseCells puzzleHouseCells)

let nakedSingleFind (cellHouseCells : MapCellHouseCells) (puzzleHouseCells : House -> Set<Cell>) 
    (candidateLookup : MapCellCandidates) (cells : Cell list) = 
    let hs = 
        List.map (fun cell -> 
            let candidates = candidateLookup.Item cell

            if Set.count candidates = 1 then 
                let candidate = first candidates

                let setCellValue = makeSetCellDigit cell candidate

                Some { HintDescription.primaryHouses = set []
                       secondaryHouses = set []
                       candidateReductions = set []
                       setCellValueAction = Some setCellValue
                       pointers = set [] }
            else None) cells
    List.choose id hs |> List.map (mhas cellHouseCells puzzleHouseCells)
