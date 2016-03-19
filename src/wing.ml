open Sset
open Smap
open Sudoku
open Puzzlemap
open Hints

let makeHints (p : puzzleMap) (cellCandidates : cellCandidates) pointerCells primaryHouses secondaryHouses candidate = 
    let pointers =
        pointerCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (Digits.singleton candidate))
        |> CandidateReductions.ofSet
        in

    let colCells =
        secondaryHouses
        |> Houses.map (SMap.get p.houseCells)
        |> Cells.unionMany
        in

    let candidatesReductions = 
        Cells.difference colCells pointerCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        |> CandidateReductions.filter (fun cr -> Digits.contains candidate cr.candidates)
        |> CandidateReductions.map (fun cr -> makeCandidateReduction cr.cell (Digits.singleton candidate))
        |> CandidateReductions.ofSet
        in

    if CandidateReductions.count candidatesReductions > 0 then 
        Some { hintDescription.candidateReductions = candidatesReductions;
               primaryHouses = primaryHouses;
               secondaryHouses = secondaryHouses;
               pointers = pointers;
               setCellValueAction = None;
               focus = Digits.empty }
    else None

let xWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) (house2 : house) (candidate : digit) = 

    let houseCandidateCells1 =
        house1
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        in

    let houseCandidateCells2 =
        house2
        |> SMap.get p.houseCells
        |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        |> CandidateReductions.ofSet
        in

    let hht1 =
        houseCandidateCells1
        |> CandidateReductions.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    let hht2 =
        houseCandidateCells2
        |> CandidateReductions.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = CandidateReductions.map (fun cr -> cr.cell.col) hht1 |> Columns.ofSet in
        let cols2 = CandidateReductions.map (fun cr -> cr.cell.col) hht2 |> Columns.ofSet in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> makeCell col row1)
                |> Cells.ofSet
                in

            let row2Cells = 
                cols
                |> Columns.map (fun col -> makeCell col row2)
                |> Cells.ofSet
                in

            let pointerCells =
                [ row1Cells; row2Cells ]
                |> Cells.unionManyList
                in

            let primaryHouses = Houses.ofList [ house1; house2 ] in

            let secondaryHouses =
                cols
                |> Columns.map HColumn
                |> Houses.ofSet
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = CandidateReductions.map (fun cr -> cr.cell.row) hht1 |> Rows.ofSet in
        let rows2 = CandidateReductions.map (fun cr -> cr.cell.row) hht2 |> Rows.ofSet in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells =
                rows
                |> Rows.map (fun row -> makeCell col1 row)
                |> Cells.ofSet
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> makeCell col2 row)
                |> Cells.ofSet
                in

            let pointerCells =
                [ col1Cells; col2Cells ]
                |> Cells.unionManyList
                in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map HRow
                |> Houses.ofSet
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) 
    (house2 : house) : hintDescription list = 

    let houseCandidates1 =
        house1
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany
        in

    let houseCandidates2 =
        house2
        |> SMap.get p.houseCells
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany
        in

    Digits.intersect houseCandidates1 houseCandidates2
    |> Digits.toList
    |> List.map (xWingsPerHouseCandidate p cellCandidates house1 house2)
    |> List.choose id

let xWings (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    let rows = List.map HRow p.rows in
    let cols = List.map HColumn p.columns in

    let rowHints1 = 
        rows
        |> List.mapi 
            (fun i row1 -> 
                Array.sub (List.toArray rows) (i + 1) (rows.Length - i - 1)
                |> Array.toList
                |> List.mapi
                    (fun j row2 -> xWingsPerHouse p cellCandidates row1 row2)) 
        in

    let rowHints = 
        rowHints1
        |> List.concat
        |> List.concat
        in

    let colHints1 = 
        cols
        |> List.mapi 
            (fun i col1 -> 
                Array.sub (List.toArray cols) (i + 1) (cols.Length - i - 1)
                |> Array.toList
                |> List.mapi
                    (fun j col2 -> xWingsPerHouse p cellCandidates col1 col2)) 
        in

    let colHints = 
        colHints1
        |> List.concat
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat

let yWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates)
    (house1 : house) (house2 : house) houseCandidateCells1 houseCandidateCells2 (candidate : digit) = 
    let hht1 =
        houseCandidateCells1
        |> CandidateReductions.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    let hht2 =
        houseCandidateCells2
        |> CandidateReductions.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = CandidateReductions.map (fun cr -> cr.cell.col) hht1 |> Columns.ofSet in
        let cols2 = CandidateReductions.map (fun cr -> cr.cell.col) hht2 |> Columns.ofSet in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> makeCell col row1)
                |> Cells.ofSet
                in

            let row2Cells =
                cols
                |> Columns.map (fun col -> makeCell col row2)
                |> Cells.ofSet
                in

            let pointerCells = Cells.union row1Cells row2Cells in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                cols
                |> Columns.map HColumn
                |> Houses.ofSet
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = CandidateReductions.map (fun cr -> cr.cell.row) hht1 |> Rows.ofSet in
        let rows2 = CandidateReductions.map (fun cr -> cr.cell.row) hht2 |> Rows.ofSet in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells = 
                rows
                |> Rows.map (fun row -> makeCell col1 row)
                |> Cells.ofSet
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> makeCell col2 row)
                |> Cells.ofSet
                in

            let pointerCells = Cells.union col1Cells col2Cells in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map HRow
                |> Houses.ofSet
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let yWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (row1 : row) 
    (row2 : row) (col1 : column) (col2 : column)  : hintDescription list = 

    let cell11 = makeCell col1 row1 in
    let cell12 = makeCell col2 row1 in
    let cell21 = makeCell col1 row2 in
    let cell22 = makeCell col2 row2 in
    
    let cells = [ cell11; cell12; cell21; cell22 ] in

    let candidateCells =
        cells
        |> List.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
        in

    let ccell11 = makeCandidateReduction cell11 (SMap.get cellCandidates cell11) in
    let ccell12 = makeCandidateReduction cell12 (SMap.get cellCandidates cell12) in
    let ccell21 = makeCandidateReduction cell21 (SMap.get cellCandidates cell21) in
    let ccell22 = makeCandidateReduction cell22 (SMap.get cellCandidates cell22) in

    let allNonEmpty =
        candidateCells
        |> List.forall (fun cr -> Digits.count cr.candidates > 0)
        in

    if allNonEmpty then 
        let triples = 
            [ (cell12, [ ccell11; ccell12; ccell22 ], cell21);
              (cell22, [ ccell12; ccell22; ccell21 ], cell11);
              (cell21, [ ccell22; ccell21; ccell11 ], cell12);
              (cell11, [ ccell21; ccell11; ccell12 ], cell22) ]
            in

        triples
        |> List.map
          (fun (pivot1, triple, other) -> 
            let ccs = List.map (fun cr -> cr.candidates) triple in

            let allPairs =
                ccs
                |> List.forall (fun c -> Digits.count c = 2)
                in

            if allPairs then 
                let allCandidates =
                    ccs
                    |> Digits.unionManyList
                    in

                if Digits.count allCandidates = 3 then 
                    match triple with
                    | [ left; pivot; right; _ ] -> 
                        let removee = Digits.difference allCandidates pivot.candidates in

                        if Digits.count removee = 1 && (left.candidates <> right.candidates) && 
                            Digits.isSubset removee (SMap.get cellCandidates other) then

                            let candidateReductions = makeCandidateReduction other removee in

                            let pointers =
                                triple
                                |> CandidateReductions.ofList
                                in

                            let primaryHouses = 
                                [ HRow row1;
                                  HRow row2;
                                  HColumn col1;
                                  HColumn col2; ]
                                |> Houses.ofList
                                in

                            let primaryHouseCells = p.housesCells primaryHouses in

                            Some { hintDescription.candidateReductions = CandidateReductions.singleton candidateReductions;
                                   primaryHouses = primaryHouses;
                                   secondaryHouses = Houses.empty;
                                   pointers = pointers;
                                   setCellValueAction = None;
                                   focus = Digits.empty }
                        else None
                    | _ -> None
                else None
            else None)
        |> List.choose id
    else List.empty

let yWings (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    let colsa = p.columns in

    let hints =
        p.rows
        |> List.mapi 
            (fun i row1 ->
                Array.sub (List.toArray p.rows) (i + 1) (p.rows.Length - i - 1)
                |> Array.toList
                |> List.mapi 
                    (fun j row2 -> 
                        colsa
                        |> List.mapi 
                            (fun k col1 -> 
                                Array.sub (List.toArray colsa) (k + 1) (colsa.Length - k - 1)
                                |> Array.toList
                                |> List.mapi
                                    (fun l col2 -> yWingsPerHouse p cellCandidates row1 row2 col1 col2)))) 
        in

    hints
    |> List.concat
    |> List.concat
    |> List.concat
    |> List.concat
