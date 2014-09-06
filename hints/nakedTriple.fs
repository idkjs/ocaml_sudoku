module hints.nakedTriple

open System
open System.Text

open console

open core.sudoku
open naked

let nakedTriplePerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 1 && Set.count candidates <= 3) candidateCells
    
    let hs = 
        Seq.mapi 
            (fun i candidatesCell1 -> 
            Seq.mapi (fun j candidatesCell2 -> 
                Seq.mapi (fun k candidatesCell3 -> 
                    let candidatesCellAll = set [ candidatesCell1; candidatesCell2; candidatesCell3 ]
                    findNaked candidatesCellAll candidateCells house) (Seq.skip (i + j + 2) hht)) (Seq.skip (i + 1) hht)) 
            hht
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let nakedTripleFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedTriplePerHouse candidateLookup houseCells) houses
