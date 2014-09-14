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

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 1 && Set.count candidates <= 3) candidateCells
    let subsets = setSubsets (Set.toList hht) count

    let hs = List.map (fun subset -> findNaked (Set.ofList subset) candidateCells house) subsets
    List.choose id hs

let nakedSingleFind (candidateLookup : Cell -> Set<Candidate>) (cells : Cell list) = 
    //List.collect (nakedNPerHouse candidateLookup houseCells 1) houses |> ignore

    let candidateCells = List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells

    List.map (fun (candidates, cell) -> 
        { HintDescription.primaryHouses = set []
          secondaryHouses = set []
          candidateReductions = set []
          setCellValue = 
              Some { SetCellValue.cell = cell
                     candidate = first candidates }
          pointers = set [] }) filteredCandidateCells
