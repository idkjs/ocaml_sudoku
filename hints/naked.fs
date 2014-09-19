module hints.naked

open System
open System.Text

open console

open core.sudoku
open hints

let findNaked (candidateLookup : Cell -> Set<Candidate>) (houseCells : Set<Cell>) cellSubset (count : int) house = 

    let symbols = Set.map candidateLookup cellSubset
    let subsetSymbols = Set.unionMany symbols

    if Set.count subsetSymbols <= count then 
        let otherCells = Set.filter (fun cell -> Set.contains cell cellSubset = false) houseCells
        
        let candidateReductions = 
            Set.map (fun cell -> 
                let candidates = candidateLookup cell
                { CandidateReduction.symbols = Set.intersect subsetSymbols candidates
                  cell = cell }) otherCells
        
        let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions
        
        let pointers = 
            Set.map (fun cell -> 
                let candidates = candidateLookup cell
                { CandidateReduction.cell = cell
                  symbols = candidates }) cellSubset

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { HintDescription.primaryHouses = set [ house ]
                   secondaryHouses = set []
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValue = None
                   pointers = pointers }

        else None
    else None

let nakedNPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (count : int) 
    (house : House) = 
    let cells = houseCells house
    
    let hht = 
        Set.filter (fun cell -> 
            let candidates = candidateLookup cell
            Set.count candidates > 1 && Set.count candidates <= count) cells
    
    let subsets = setSubsets (Set.toList hht) count

    let hs = List.map (fun subset -> findNaked candidateLookup cells (Set.ofList subset) count house) subsets

    List.choose id hs

let nakedSingleFind (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 
    let hs = 
        List.map (fun cell -> 
            let candidates = candidateLookup cell
            if Set.count candidates = 1 then 
                Some { HintDescription.primaryHouses = set []
                       secondaryHouses = set []
                       candidateReductions = set []
                       setCellValue = 
                           Some { SetCellValue.cell = cell
                                  candidate = first candidates }
                       pointers = set [] }
            else None) cells
    List.choose id hs
