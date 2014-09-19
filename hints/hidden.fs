module hints.hidden

open System
open System.Text

open console

open core.setCell
open core.sudoku
open hints

let findHidden (candidateLookup : Cell -> Set<Candidate>) (houseCells : List<Cell>) (candidateSubset : Set<Candidate>) 
    (count : int) house = 
    let pairs = 
        List.map (fun cell -> 
            let candidates = candidateLookup cell
            
            let pointer = 
                { CandidateReduction.cell = cell
                  symbols = Set.intersect candidates candidateSubset }
            
            let crs = 
                if Set.count pointer.symbols > 0 then Set.difference candidates candidateSubset
                else set []
            
            let candidateReduction = 
                { CandidateReduction.cell = cell
                  symbols = crs }
            
            (pointer, candidateReduction)) houseCells
    
    let pointers, candidateReductions = List.unzip pairs

    let nonEmptyPointers = List.filter (fun cr -> Set.count cr.symbols > 0) pointers

    let nonEmptyCandidateReductions = List.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions

    if List.length nonEmptyPointers = count && List.length nonEmptyCandidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = List.head nonEmptyPointers
                Some { SetCellValue.cell = h.cell
                       candidate = first candidateSubset }
            else None

        Some { HintDescription.primaryHouses = set [ house ]
               secondaryHouses = set []
               candidateReductions = Set.ofList nonEmptyCandidateReductions
               setCellValue = setCellValue
               pointers = Set.ofList nonEmptyPointers }
    else None

let hiddenNPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (count : int) 
    (house : House) = 
    let cells = houseCells house

    let houseCandidates = Set.map candidateLookup cells |> Set.unionMany

    let candidateSubsets = setSubsets (Set.toList houseCandidates) count
    let hs = 
        List.map 
            (fun candidateSubset -> 
            findHidden candidateLookup (Set.toList cells) (Set.ofList candidateSubset) count house) candidateSubsets
    List.choose id hs
