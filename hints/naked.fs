module hints.naked

open System
open System.Text

open console

open core.sudoku
open hints

let findNaked candidatesCellAll candidateCells house = 
    let symbols = Set.map fst candidatesCellAll
    let scells = Set.map snd candidatesCellAll
    let quad = Set.unionMany symbols
    if Set.count quad <= Set.count candidatesCellAll then 
        let chosenCells, otherCells = Set.partition (fun (_, cell) -> Set.contains cell scells) candidateCells
        
        // find any reductions
        let candidateReductions = 
            Set.map (fun (candidatesrc, cellrc) -> 
                { CandidateReduction.symbols = Set.intersect quad candidatesrc
                  cell = cellrc }) otherCells
        
        let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions
        
        let pointers = 
            Set.map (fun (candidates, cell) -> 
                { CandidateReduction.cell = cell
                  symbols = candidates }) chosenCells

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { HintDescription.house = Some house
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValue = None
                   pointers = pointers }

        else None
    else None
