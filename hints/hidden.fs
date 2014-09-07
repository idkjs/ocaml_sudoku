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
                Some { HintDescription.house = Some house
                       candidateReductions = candidateReductions
                       setCellValue = setCellValue
                       pointers = pointers }
            else None
        else None
    else None


let hiddenSinglesPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi (fun i symbol1 -> 
            let symbols = set [ symbol1 ]

            findHidden symbols houseCandidates candidateCells house) salphabet
    
    let hhs = hs

    Seq.choose id hhs |> Seq.toList

let hiddenSingleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenSinglesPerHouse alphabet candidateLookup houseCells) houses


let hiddenPairsPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi (fun i symbol1 -> 
            Seq.mapi (fun j symbol2 -> 
                let symbols = set [ symbol1; symbol2 ]

                findHidden symbols houseCandidates candidateCells house) (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = Seq.concat hs

    Seq.choose id hhs |> Seq.toList

let hiddenPairFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenPairsPerHouse alphabet candidateLookup houseCells) houses


let hiddenTriplesPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi 
            (fun i symbol1 -> 
            Seq.mapi (fun j symbol2 -> 
                Seq.mapi (fun k symbol3 -> 
                    let symbols = set [ symbol1; symbol2; symbol3 ]

                    findHidden symbols houseCandidates candidateCells house) (Seq.skip (i + j + 2) salphabet)) 
                (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let hiddenTripleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenTriplesPerHouse alphabet candidateLookup houseCells) houses


let hiddenQuadsPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi 
            (fun i symbol1 -> 
            Seq.mapi 
                (fun j symbol2 -> 
                Seq.mapi (fun k symbol3 -> 
                    Seq.mapi (fun l symbol4 -> 
                        let symbols = set [ symbol1; symbol2; symbol3; symbol4 ]

                        findHidden symbols houseCandidates candidateCells house) (Seq.skip (i + j + k + 3) salphabet)) 
                    (Seq.skip (i + j + 2) salphabet)) (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let hiddenQuadFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenQuadsPerHouse alphabet candidateLookup houseCells) houses



