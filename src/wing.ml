open Sudoku
open Puzzlemap
(*F# open FSharp.Compatibility.OCaml F#*)

let makeHints (p : puzzleMap) (cellCandidates : cellCandidates) pointerCells primaryHouses secondaryHouses candidate : Hint.description option = 
    let pointers =
        pointerCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
        in

    let colCells =
        secondaryHouses
        |> Houses.map (Smap.get House.comparer p.houseCells)
        |> Cells.unionManyList
        in

    let candidatesReductions = 
        Cells.difference colCells pointerCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        |> List.map (fun cr -> CandidateReduction.make cr.cell (Digits.singleton candidate))
        in

    if List.length candidatesReductions > 0 then 
        Some { primaryHouses = primaryHouses;
               secondaryHouses = secondaryHouses;
               candidateReductions = candidatesReductions;
               setCellValueAction = None;
               pointers = pointers;
               focus = Digits.empty }
    else None

let xWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) (house2 : house) (candidate : digit) = 

    let houseCandidateCells1 =
        house1
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
        in

    let houseCandidateCells2 =
        house2
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
        in

    let hht1 =
        houseCandidateCells1
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    let hht2 =
        houseCandidateCells2
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = List.map (fun cr -> cr.cell.col) hht1 |> Columns.ofList in
        let cols2 = List.map (fun cr -> cr.cell.col) hht2 |> Columns.ofList in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row1)
                |> Cells.ofList
                in

            let row2Cells = 
                cols
                |> Columns.map (fun col -> Cell.make col row2)
                |> Cells.ofList
                in

            let pointerCells =
                [ row1Cells; row2Cells ]
                |> Cells.unionManyList
                in

            let primaryHouses = Houses.ofList [ house1; house2 ] in

            let secondaryHouses =
                cols
                |> Columns.map House.make_column
                |> Houses.ofList
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = List.map (fun cr -> cr.cell.row) hht1 |> Rows.ofList in
        let rows2 = List.map (fun cr -> cr.cell.row) hht2 |> Rows.ofList in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells =
                rows
                |> Rows.map (fun row -> Cell.make col1 row)
                |> Cells.ofList
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> Cell.make col2 row)
                |> Cells.ofList
                in

            let pointerCells =
                [ col1Cells; col2Cells ]
                |> Cells.unionManyList
                in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map House.make_row
                |> Houses.ofList
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) 
    (house2 : house) : Hint.description list = 

    let houseCandidates1 =
        house1
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (CellCandidates.get cellCandidates)
        |> Digits.unionManyList
        in

    let houseCandidates2 =
        house2
        |> Smap.get House.comparer p.houseCells
        |> Cells.map (CellCandidates.get cellCandidates)
        |> Digits.unionManyList
        in

    Digits.intersect houseCandidates1 houseCandidates2
    |> Digits.toList
    |> List.map (xWingsPerHouseCandidate p cellCandidates house1 house2)
    |> Sset.choose Sset.id

let xWings (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    let rows = Rows.map House.make_row p.rows |> Houses.ofList in
    let cols = Columns.map House.make_column p.columns |> Houses.ofList in

    let rowHints1 = 
        rows
        |> Houses.mapi 
            (fun i row1 -> 
                Houses.drop (i + 1) rows
                |> Houses.mapi
                    (fun j row2 -> xWingsPerHouse p cellCandidates row1 row2)) 
        in

    let rowHints = 
        rowHints1
        |> List.concat
        |> List.concat
        in

    let colHints1 = 
        cols
        |> Houses.mapi
            (fun i col1 -> 
                Houses.drop (i + 1) cols
                |> Houses.mapi
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
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    let hht2 =
        houseCandidateCells2
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = List.map (fun cr -> cr.cell.col) hht1 |> Columns.ofList in
        let cols2 = List.map (fun cr -> cr.cell.col) hht2 |> Columns.ofList in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row1)
                |> Cells.ofList
                in

            let row2Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row2)
                |> Cells.ofList
                in

            let pointerCells = Cells.union row1Cells row2Cells in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                cols
                |> Columns.map House.make_column
                |> Houses.ofList
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = List.map (fun cr -> cr.cell.row) hht1 |> Rows.ofList in
        let rows2 = List.map (fun cr -> cr.cell.row) hht2 |> Rows.ofList in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells = 
                rows
                |> Rows.map (fun row -> Cell.make col1 row)
                |> Cells.ofList
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> Cell.make col2 row)
                |> Cells.ofList
                in

            let pointerCells = Cells.union col1Cells col2Cells in

            let primaryHouses = Houses.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map House.make_row
                |> Houses.ofList
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let yWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (row1 : row) 
    (row2 : row) (col1 : column) (col2 : column)  : Hint.description list = 

    let cell11 = Cell.make col1 row1 in
    let cell12 = Cell.make col2 row1 in
    let cell21 = Cell.make col1 row2 in
    let cell22 = Cell.make col2 row2 in
    
    let cells = [ cell11; cell12; cell21; cell22 ] in

    let candidateCells =
        cells
        |> List.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cellCandidates cell))
        in

    let ccell11 = CandidateReduction.make cell11 (CellCandidates.get cellCandidates cell11) in
    let ccell12 = CandidateReduction.make cell12 (CellCandidates.get cellCandidates cell12) in
    let ccell21 = CandidateReduction.make cell21 (CellCandidates.get cellCandidates cell21) in
    let ccell22 = CandidateReduction.make cell22 (CellCandidates.get cellCandidates cell22) in

    let allNonEmpty =
        candidateCells
        |> List.for_all (fun cr -> Digits.count cr.candidates > 0)
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
                |> List.for_all (fun c -> Digits.count c = 2)
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
                            Digits.isSubset removee (CellCandidates.get cellCandidates other) then

                            let candidateReductions = CandidateReduction.make other removee in

                            let pointers = triple in

                            let primaryHouses = 
                                [ HRow row1;
                                  HRow row2;
                                  HColumn col1;
                                  HColumn col2; ]
                                |> Houses.ofList
                                in

                            let primaryHouseCells = p.housesCells primaryHouses in

                            let desc : Hint.description =
                                { primaryHouses = primaryHouses;
                                  secondaryHouses = Houses.empty;
                                  candidateReductions = [candidateReductions];
                                  setCellValueAction = None;
                                  pointers = pointers;
                                  focus = Digits.empty } in
                            Some desc
                        else None
                    | _ -> None
                else None
            else None)
        |> Sset.choose Sset.id
    else []

let yWings (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    let hints =
        p.rows
        |> Rows.mapi 
            (fun i row1 ->
                Rows.drop (i + 1) p.rows
                |> Rows.mapi 
                    (fun j row2 -> 
                        p.columns
                        |> Columns.mapi 
                            (fun k col1 -> 
                                Columns.drop (k + 1) p.columns
                                |> Columns.mapi
                                    (fun l col2 -> yWingsPerHouse p cellCandidates row1 row2 col1 col2)))) 
        in

    hints
    |> List.concat
    |> List.concat
    |> List.concat
    |> List.concat
