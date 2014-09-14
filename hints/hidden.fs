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
            
            let setCellValue = 
                if Set.count a = 1 then 
                    let scv = first a
                    Some { SetCellValue.cell = snd scv
                           candidate = first symbols }
                else None
            
            let nonEmptyCandidateReductions = Set.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions
            
            let pointers = 
                Set.map (fun (candidates, cell) -> 
                    { CandidateReduction.cell = cell
                      symbols = Set.intersect candidates symbols }) a

            if Set.count nonEmptyCandidateReductions > 0 then 
                Some { HintDescription.primaryHouses = set [ house ]
                       secondaryHouses = set []
                       candidateReductions = candidateReductions
                       setCellValue = setCellValue
                       pointers = pointers }
            else None
        else None
    else None

let hiddenNPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (count : int) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let subsets = setSubsets alphabet count

    let hs = List.map (fun symbols -> findHidden (Set.ofList symbols) houseCandidates candidateCells house) subsets

    List.choose id hs

let hiddenSingleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenNPerHouse alphabet candidateLookup houseCells 1) houses

let hiddenPairFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenNPerHouse alphabet candidateLookup houseCells 2) houses

let hiddenTripleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenNPerHouse alphabet candidateLookup houseCells 3) houses

let hiddenQuadFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenNPerHouse alphabet candidateLookup houseCells 4) houses
