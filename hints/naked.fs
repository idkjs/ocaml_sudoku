module hints.naked

open System
open System.Text

open console

open core.setCell
open core.sudoku
open hints

let findNaked (cellHouseCells : Cell -> Set<Cell>) (candidateLookup : Cell -> Set<Candidate>) (primaryHouseCells : Set<Cell>) (cellSubset : Set<Cell>) (count : int) (primaryHouse : House) = 

    let symbols = Set.map candidateLookup cellSubset
    let subsetSymbols = Set.unionMany symbols

    if Set.count subsetSymbols <= count then 
        let otherCells = Set.filter (fun cell -> Set.contains cell cellSubset = false) primaryHouseCells
        
        let candidateReductions = 
            Set.map (fun cell -> 
                let candidates = candidateLookup cell
                { CandidateReduction.candidates = Set.intersect subsetSymbols candidates
                  cell = cell }) otherCells
        
        let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.candidates > 0) candidateReductions
        
        let pointers = 
            Set.map (fun cell -> 
                let candidates = candidateLookup cell
                { CandidateReduction.cell = cell
                  candidates = candidates }) cellSubset

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { HintDescription.primaryHouses = set [ primaryHouse ]
                   secondaryHouses = set []
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValue = None
                   pointers = pointers }

        else None
    else None

let nakedNPerHouse (cellHouseCells : Cell -> Set<Cell>) (puzzleHouseCells : House -> Set<Cell>) (candidateLookup : Cell -> Set<Candidate>) (count : int) 
    (primaryHouse : House) = 

    let primaryHouseCells = puzzleHouseCells primaryHouse
    
    let hht = 
        Set.filter (fun cell -> 
            let candidates = candidateLookup cell
            Set.count candidates > 1 && Set.count candidates <= count) primaryHouseCells
    
    let subsets = setSubsets (Set.toList hht) count

    let hs = List.map (fun subset -> findNaked cellHouseCells candidateLookup primaryHouseCells (Set.ofList subset) count primaryHouse) subsets

    List.choose id hs |> List.map (mhas cellHouseCells puzzleHouseCells)

let nakedSingleFind (cellHouseCells : Cell -> Set<Cell>) (puzzleHouseCells : House -> Set<Cell>) (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 
    let hs = 
        List.map (fun cell -> 
            let candidates = candidateLookup cell

            if Set.count candidates = 1 then 
                let candidate = first candidates

                let setCellValue = makeSetCellSymbol cell candidate

                Some { HintDescription.primaryHouses = set []
                       secondaryHouses = set []
                       candidateReductions = set []
                       setCellValue = Some setCellValue
                       pointers = set [] }
            else None) cells
    List.choose id hs |> List.map (mhas cellHouseCells puzzleHouseCells)
