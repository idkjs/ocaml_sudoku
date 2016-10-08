open Sudoku
open Puzzlemap
open Hint
(*F# open FSharp.Compatibility.OCaml F#*)

let makeHints (p : puzzleMap) (cellCandidates : cellCandidates) pointerCells primaryHouses secondaryHouses candidate : Hint.description option = 
    let pointers =
        pointerCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
        in

    let colCells =
        secondaryHouses
        |> Houses.map (fun house -> Smap.get House.comparer house p.houseCells)
        |> Cells.union_many
        in

    let candidatesReductions = 
        Cells.difference colCells pointerCells
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        |> List.filter (fun cr -> Digits.contains candidate cr.candidates)
        |> List.map (fun cr -> CandidateReduction.make cr.cell (Digits.singleton candidate))
        in

    if List.length candidatesReductions > 0 then
        let hint : Hint.description =
            { primaryHouses = primaryHouses;
              secondaryHouses = secondaryHouses;
              candidateReductions = candidatesReductions;
              setCellValueAction = None;
              pointers = pointers;
              focus = Digits.empty }
            in
        Some hint
    else None

let xWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) (house2 : house) (candidate : digit) = 

    let houseCandidateCells1 =
        p.houseCells
        |> Smap.get House.comparer house1
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        in

    let houseCandidateCells2 =
        p.houseCells
        |> Smap.get House.comparer house2
        |> Cells.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
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
        let cols1 = List.map (fun cr -> cr.cell.col) hht1 |> Columns.make in
        let cols2 = List.map (fun cr -> cr.cell.col) hht2 |> Columns.make in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row1)
                |> Cells.make
                in

            let row2Cells = 
                cols
                |> Columns.map (fun col -> Cell.make col row2)
                |> Cells.make
                in

            let pointerCells =
                [ row1Cells; row2Cells ]
                |> Cells.union_many
                in

            let primaryHouses = Houses.make [ house1; house2 ] in

            let secondaryHouses =
                cols
                |> Columns.map House.make_column
                |> Houses.make
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = List.map (fun cr -> cr.cell.row) hht1 |> Rows.make in
        let rows2 = List.map (fun cr -> cr.cell.row) hht2 |> Rows.make in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells =
                rows
                |> Rows.map (fun row -> Cell.make col1 row)
                |> Cells.make
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> Cell.make col2 row)
                |> Cells.make
                in

            let pointerCells =
                [ col1Cells; col2Cells ]
                |> Cells.union_many
                in

            let primaryHouses = Houses.make [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map House.make_row
                |> Houses.make
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) 
    (house2 : house) : Hint.description list = 

    let houseCandidates1 =
        p.houseCells
        |> Smap.get House.comparer house1
        |> Cells.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> Digits.union_many
        in

    let houseCandidates2 =
        p.houseCells
        |> Smap.get House.comparer house2
        |> Cells.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> Digits.union_many
        in

    Digits.intersect houseCandidates1 houseCandidates2
    |> Digits.map (xWingsPerHouseCandidate p cellCandidates house1 house2)
    |> Sset.choose Sset.id

let xWings (p : puzzleMap) (cellCandidates : cellCandidates) : Hint.description list =
    let rows = Rows.map House.make_row p.rows |> Houses.make in
    let cols = Columns.map House.make_column p.columns |> Houses.make in

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
        let cols1 = List.map (fun cr -> cr.cell.col) hht1 |> Columns.make in
        let cols2 = List.map (fun cr -> cr.cell.col) hht2 |> Columns.make in

        let cols = Columns.union cols1 cols2 in

        if Columns.count cols1 = 2 && Columns.count cols2 = 2 && Columns.count cols = 2 then 
            let row1Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row1)
                |> Cells.make
                in

            let row2Cells =
                cols
                |> Columns.map (fun col -> Cell.make col row2)
                |> Cells.make
                in

            let pointerCells = Cells.union row1Cells row2Cells in

            let primaryHouses = Houses.make [ house1; house2 ] in
            let secondaryHouses =
                cols
                |> Columns.map House.make_column
                |> Houses.make
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = List.map (fun cr -> cr.cell.row) hht1 |> Rows.make in
        let rows2 = List.map (fun cr -> cr.cell.row) hht2 |> Rows.make in

        let rows = Rows.union rows1 rows2 in

        if Rows.count rows1 = 2 && Rows.count rows2 = 2 && Rows.count rows = 2 then 
            let col1Cells = 
                rows
                |> Rows.map (fun row -> Cell.make col1 row)
                |> Cells.make
                in

            let col2Cells =
                rows
                |> Rows.map (fun row -> Cell.make col2 row)
                |> Cells.make
                in

            let pointerCells = Cells.union col1Cells col2Cells in

            let primaryHouses = Houses.make [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> Rows.map House.make_row
                |> Houses.make
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
        |> List.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        in

    let ccell11 = CandidateReduction.make cell11 (CellCandidates.get cell11 cellCandidates) in
    let ccell12 = CandidateReduction.make cell12 (CellCandidates.get cell12 cellCandidates) in
    let ccell21 = CandidateReduction.make cell21 (CellCandidates.get cell21 cellCandidates) in
    let ccell22 = CandidateReduction.make cell22 (CellCandidates.get cell22 cellCandidates) in

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
                    |> Digits.union_many
                    in

                if Digits.count allCandidates = 3 then 
                    match triple with
                    | [ left; pivot; right; _ ] -> 
                        let removee = Digits.difference allCandidates pivot.candidates in

                        if Digits.count removee = 1 && (left.candidates <> right.candidates) && 
                            Digits.is_subset removee (CellCandidates.get other cellCandidates) then

                            let candidateReductions = CandidateReduction.make other removee in

                            let pointers = triple in

                            let primaryHouses = 
                                [ HRow row1;
                                  HRow row2;
                                  HColumn col1;
                                  HColumn col2; ]
                                |> Houses.make
                                in

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
