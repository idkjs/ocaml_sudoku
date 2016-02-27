module hints.naked

open core.sudoku
open core.puzzlemap
open core.hints

let findNaked (count : int) (cellHouseCells : cellHouseCells) (candidateLookup : cellCandidates) 
    (primaryHouseCells : Set<cell>) (cellSubset : Set<cell>) (primaryHouse : house) = 

    let subsetDigits =
        cellSubset
        |> Set.map candidateLookup.Get
        |> Set.unionMany

    if Set.count subsetDigits <= count then 
        let otherCells =
            primaryHouseCells
            |> Set.filter (fun cell -> Set.contains cell cellSubset = false) 

        let candidateReductions =
            otherCells
            |> Set.map (fun cell -> 
                let candidates = candidateLookup.Get cell
                { candidateReduction.candidates = Set.intersect subsetDigits candidates
                  cell = cell }) 
        
        let nonEmptyCandidateReductions =
            candidateReductions
            |> Set.filter (fun cr -> Set.count cr.candidates > 0)
        
        let pointers =
            cellSubset
            |> Set.map (fun cell -> 
                let candidates = candidateLookup.Get cell
                { candidateReduction.cell = cell
                  candidates = candidates })

        if Set.count nonEmptyCandidateReductions > 0 then 
            Some { hintDescription.primaryHouses = set [ primaryHouse ]
                   secondaryHouses = set []
                   candidateReductions = nonEmptyCandidateReductions
                   setCellValueAction = None
                   pointers = pointers
                   focus = set [] }

        else None
    else None

let nakedNPerHouse (count : int) (p : puzzleMap) (candidateLookup : cellCandidates)  (primaryHouse : house) : Set<hintDescription> =
    let primaryHouseCells =
        primaryHouse
        |> p.houseCells.Get
    
    let hht = 
        primaryHouseCells
        |> Set.filter (fun cell -> 
            let candidates = candidateLookup.Get cell
            Set.count candidates > 1 && Set.count candidates <= count) 
    
    setSubsets (Set.toList hht) count
    |> Set.ofList
    |> Set.map 
        (fun subset -> 
        findNaked count p.cellHouseCells candidateLookup primaryHouseCells (Set.ofList subset) primaryHouse)
    |> Set.filter Option.isSome
    |> Set.map Option.get

let nakedSingle (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =

    p.cells
    |> Set.map (fun cell -> 
        let candidates = candidateLookup.Get cell

        if Set.count candidates = 1 then 
            let candidate = first candidates

            let setCellValue = makeSetCellDigit cell candidate

            Some { hintDescription.primaryHouses = set []
                   secondaryHouses = set []
                   candidateReductions = set []
                   setCellValueAction = Some setCellValue
                   pointers = set []
                   focus = set [] }
        else None)
    |> Set.filter Option.isSome
    |> Set.map Option.get

let nakedN (i : int) (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    p.houses
    |> Set.map (nakedNPerHouse i p candidateLookup )
    |> Set.unionMany
