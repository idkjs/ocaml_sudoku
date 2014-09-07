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

let nakedSingleFind (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 

    let candidateCells = List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells

    List.map (fun (candidates, cell) -> 
        { HintDescription.house = None
          candidateReductions = set []
          setCellValue = 
              Some { SetCellValue.cell = cell
                     candidate = first candidates }
          pointers = set [] }) filteredCandidateCells

let nakedPairPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 1 && Set.count candidates <= 2) candidateCells
    
    let hs = 
        Seq.mapi (fun i candidatesCell1 -> 
            Seq.mapi (fun j candidatesCell2 -> 
                let candidatesCellAll = set [ candidatesCell1; candidatesCell2 ]
                findNaked candidatesCellAll candidateCells house) (Seq.skip (i + 1) hht)) hht
    
    let hhs = hs |> Seq.concat
    Seq.choose id hhs |> Seq.toList

let nakedPairFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedPairPerHouse candidateLookup houseCells) houses


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


let nakedQuadPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 1 && Set.count candidates <= 4) candidateCells
    
    let hs = 
        Seq.mapi 
            (fun i candidatesCell1 -> 
            Seq.mapi 
                (fun j candidatesCell2 -> 
                Seq.mapi (fun k candidatesCell3 -> 
                    Seq.mapi (fun k candidatesCell4 -> 
                        let candidatesCellAll = 
                            set [ candidatesCell1; candidatesCell2; candidatesCell3; candidatesCell4 ]
                        findNaked candidatesCellAll candidateCells house) (Seq.skip (i + j + k + 3) hht)) 
                    (Seq.skip (i + j + 2) hht)) (Seq.skip (i + 1) hht)) hht
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let nakedQuadFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedQuadPerHouse candidateLookup houseCells) houses

