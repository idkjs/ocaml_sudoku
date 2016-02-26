module hints.wing

open core.sudoku
open core.puzzlemap
open core.hints

let makeHints (candidateLookup : cellCandidates) (puzzleHouseCells : houseCells) pointerCells primaryHouses secondaryHouses candidate = 
    let pointers =
        pointerCells
        |> Set.map (fun cell -> 
            { candidateReduction.cell = cell
              candidates = set [ candidate ] }) 
    
    let colCells =
        secondaryHouses
        |> Set.map puzzleHouseCells.Get
        |> Set.unionMany

    let candidatesReductions = 
        Set.difference colCells pointerCells
        |> Set.map (fun cell -> (candidateLookup.Get cell, cell))
        |> Set.filter (fun (candidates, _) -> Set.contains candidate candidates)
        |> Set.map (fun (_, cell) -> 
            { candidateReduction.cell = cell
              candidates = set [ candidate ] }) 

    if Set.count candidatesReductions > 0 then 
        Some { hintDescription.candidateReductions = candidatesReductions
               primaryHouses = primaryHouses
               secondaryHouses = secondaryHouses
               pointers = pointers
               setCellValueAction = None
               focus = set [] }
    else None

let xWingsPerHouseCandidate (candidateLookup : cellCandidates) (puzzleHouseCells : houseCells) 
    (house1 : house) (house2 : house) houseCandidateCells1 houseCandidateCells2 (candidate : digit) = 
    let hht1 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells1
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells2

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = Set.map (fun (_, cell) -> cell.col) hht1
        let cols2 = Set.map (fun (_, cell) -> cell.col) hht2

        let cols = Set.union cols1 cols2

        if Set.count cols1 = 2 && Set.count cols2 = 2 && Set.count cols = 2 then 
            let row1Cells = 
                Set.map (fun col -> 
                    { cell.col = col
                      row = row1 }) cols
            
            let row2Cells = 
                Set.map (fun col -> 
                    { cell.col = col
                      row = row2 }) cols
            
            let pointerCells = Set.union row1Cells row2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map HColumn cols

            makeHints candidateLookup puzzleHouseCells pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = Set.map (fun (_, cell) -> cell.row) hht1
        let rows2 = Set.map (fun (_, cell) -> cell.row) hht2

        let rows = Set.union rows1 rows2

        if Set.count rows1 = 2 && Set.count rows2 = 2 && Set.count rows = 2 then 
            let col1Cells = 
                Set.map (fun row -> 
                    { cell.col = col1
                      row = row }) rows
            
            let col2Cells = 
                Set.map (fun row -> 
                    { cell.col = col2
                      row = row }) rows
            
            let pointerCells = Set.union col1Cells col2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map HRow rows

            makeHints candidateLookup puzzleHouseCells pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (candidateLookup : cellCandidates) (puzzleHouseCells : houseCells) (house1 : house) 
    (house2 : house) = 

    let candidateCells1 =
        house1
        |> puzzleHouseCells.Get
        |> Set.map (fun cell -> ((candidateLookup.Get cell), cell)) 

    let houseCandidates1 =
        candidateCells1
        |> Set.map fst
        |> Set.unionMany

    let candidateCells2 =
        house2
        |> puzzleHouseCells.Get
        |> Set.map (fun cell -> ((candidateLookup.Get cell), cell)) 

    let houseCandidates2 =
        candidateCells2
        |> Set.map fst
        |> Set.unionMany

    let commonHouseCandidates = Set.intersect houseCandidates1 houseCandidates2

    commonHouseCandidates
    |> Seq.map (xWingsPerHouseCandidate candidateLookup puzzleHouseCells house1 house2 candidateCells1 candidateCells2) 

let xWings (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    let rows : Set<house> =
        p.rows
        |> Set.map HRow

    let cols : Set<house> =
        p.columns
        |> Set.map HColumn

    let rowHints1 = 
        rows
        |> Seq.mapi 
            (fun i row1 -> 
                Seq.mapi (fun j row2 -> xWingsPerHouse candidateLookup p.houseCells row1 row2) (Seq.skip (i + 1) rows)) 
    
    let rowHints = 
        rowHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Set.ofSeq

    let colHints1 = 
        Seq.mapi 
            (fun i col1 -> 
            Seq.mapi (fun j col2 -> xWingsPerHouse candidateLookup p.houseCells col1 col2) (Seq.skip (i + 1) cols)) 
            cols
    
    let colHints = 
        colHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList
        |> Set.ofSeq

    [ rowHints; colHints ]
    |> Set.unionMany

let yWingsPerHouseCandidate (candidateLookup : cellCandidates) (puzzleHouseCells : houseCells) 
    (house1 : house) (house2 : house) houseCandidateCells1 houseCandidateCells2 (candidate : digit) = 
    let hht1 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells1
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells2

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = Set.map (fun (_, cell) -> cell.col) hht1
        let cols2 = Set.map (fun (_, cell) -> cell.col) hht2

        let cols = Set.union cols1 cols2

        if Set.count cols1 = 2 && Set.count cols2 = 2 && Set.count cols = 2 then 
            let row1Cells = 
                Set.map (fun col -> 
                    { cell.col = col
                      row = row1 }) cols
            
            let row2Cells = 
                Set.map (fun col -> 
                    { cell.col = col
                      row = row2 }) cols
            
            let pointerCells = Set.union row1Cells row2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map HColumn cols

            makeHints candidateLookup puzzleHouseCells pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = Set.map (fun (_, cell) -> cell.row) hht1
        let rows2 = Set.map (fun (_, cell) -> cell.row) hht2

        let rows = Set.union rows1 rows2
        if Set.count rows1 = 2 && Set.count rows2 = 2 && Set.count rows = 2 then 
            let col1Cells = 
                Set.map (fun row -> 
                    { cell.col = col1
                      row = row }) rows
            
            let col2Cells = 
                Set.map (fun row -> 
                    { cell.col = col2
                      row = row }) rows
            
            let pointerCells = Set.union col1Cells col2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map HRow rows

            makeHints candidateLookup puzzleHouseCells pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let yWingsPerHouse (candidateLookup : cellCandidates) (puzzleHouseCells : houseCells) (row1 : row) 
    (row2 : row) (col1 : column) (col2 : column) = 
    let cell11 = 
        { cell.col = col1
          row = row1 }
    
    let cell12 = 
        { cell.col = col2
          row = row1 }
    
    let cell21 = 
        { cell.col = col1
          row = row2 }
    
    let cell22 = 
        { cell.col = col2
          row = row2 }
    
    let cells = [ cell11; cell12; cell21; cell22 ]

    let candidateCells = List.map (fun cell -> ((candidateLookup.Get cell), cell)) cells

    let ccell11 = ((candidateLookup.Get cell11), cell11)
    let ccell12 = ((candidateLookup.Get cell12), cell12)
    let ccell21 = ((candidateLookup.Get cell21), cell21)
    let ccell22 = ((candidateLookup.Get cell22), cell22)

    let allNonEmpty = List.forall (fun (c, _) -> Set.count c > 0) candidateCells

    if allNonEmpty then 
        let triples = 
            [ (cell12, [ ccell11; ccell12; ccell22 ], cell21)
              (cell22, [ ccell12; ccell22; ccell21 ], cell11)
              (cell21, [ ccell22; ccell21; ccell11 ], cell12)
              (cell11, [ ccell21; ccell11; ccell12 ], cell22) ]

        List.map (fun (pivot1, triple, other) -> 
            let ccs = List.map fst triple

            let allPairs = List.forall (fun c -> Set.count c = 2) ccs

            if allPairs then 
                let allCandidates = Set.unionMany ccs

                if Set.count allCandidates = 3 then 
                    match triple with
                    | left :: pivot :: right :: _ -> 
                        let removee = Set.difference allCandidates (fst pivot)

                        if Set.count removee = 1 && ((fst left) <> (fst right)) && 
                            Set.isSubset removee (candidateLookup.Get other) then

                            let candidateReductions = { candidateReduction.cell = other; candidates = removee }

                            let pointers = List.map (fun (cr, cell) -> { candidateReduction.cell = cell; candidates = cr } ) triple
                                           |> Set.ofList

                            let primaryHouses = 
                                set [ HRow row1
                                      HRow row2
                                      HColumn col1
                                      HColumn col2 ]
                            
                            let primaryHouseCells =
                                primaryHouses
                                |> Set.map puzzleHouseCells.Get
                                |> Set.unionMany

                            Some { hintDescription.candidateReductions = set [ candidateReductions ]
                                   primaryHouses = primaryHouses
                                   secondaryHouses = set []
                                   pointers = pointers
                                   setCellValueAction = None
                                   focus = set [] }
                        else None
                    | _ -> None
                else None
            else None) triples
    else []

let yWings (p : puzzleMap) (candidateLookup : cellCandidates) : Set<hintDescription> =
    let hints = 
        Seq.mapi 
            (fun i row1 -> 
            Seq.mapi 
                (fun j row2 -> 
                Seq.mapi 
                    (fun k col1 -> 
                    Seq.mapi (fun l col2 -> yWingsPerHouse candidateLookup p.houseCells row1 row2 col1 col2) 
                        (Seq.skip (k + 1) p.columns)) p.columns) (Seq.skip (i + 1) p.rows)) p.rows

    hints
    |> Seq.concat
    |> Seq.concat
    |> Seq.concat
    |> Seq.concat
    |> Seq.choose id
    |> Set.ofSeq
