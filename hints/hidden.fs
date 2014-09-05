module hints.hidden

open System
open System.Text

open console

open core.setCell
open core.sudoku
open hints

let findHidden symbols houseCandidates candidateCells house = 
    if Set.isSubset symbols houseCandidates then 
        let fcc = 
            Set.map (fun symbol -> Set.filter (fun (candidates, _) -> Set.contains symbol candidates) candidateCells) 
                symbols

        let a = Set.unionMany fcc

        if Set.count a = Set.count symbols then 

            let candidateReductions = 
                Set.map (fun (candidates, cell) -> 
                    { CandidateReduction.cell = cell
                      symbols = Set.difference candidates symbols }) a
            
            let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions
            
            let pointers = 
                Set.map (fun (candidates, cell) -> 
                    { CandidateReduction.cell = cell
                      symbols = Set.intersect candidates symbols }) a

            if Set.count nonEmptyCandidateReductions > 0 then 
                Some { HintDescription.house = Some house
                       candidateReductions = candidateReductions
                       setCellValue = None
                       pointers = pointers }
            else None
        else None
    else None
