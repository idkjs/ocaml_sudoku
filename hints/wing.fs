module hints.wing

open core.puzzlemap
open core.sudoku
open hints

let xWingsPerHouseCandidate (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house1 : House) (house2 : House) houseCandidateCells1 houseCandidateCells2 (candidate : Candidate) = 
    let hht1 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells1
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells2

    let makeHints pointerCells secondaryHouses =
        let pointers = Set.map (fun cell -> { CandidateReduction.cell = cell; symbols = set [ candidate ] } ) pointerCells

        let colCells = Set.map houseCells secondaryHouses |> Set.unionMany
        let secondaryCells = Set.difference colCells pointerCells

        let colCandidatesCells = Set.map (fun cell -> (candidateLookup cell, cell)) secondaryCells
        let potentialCells = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) colCandidatesCells
        let candidatesReductions = Set.map (fun (_, cell) -> { CandidateReduction.cell = cell; symbols = set [ candidate ] } ) potentialCells

        Some { HintDescription.candidateReductions = candidatesReductions
               primaryHouses = set [ house1; house2 ]
               secondaryHouses = secondaryHouses
               pointers = pointers
               setCellValue = None }

    match house1, house2 with
    | Row row1, Row row2 ->
        let cols1 = Set.map (fun (_, cell) -> cell.col) hht1
        let cols2 = Set.map (fun (_, cell) -> cell.col) hht2

        let cols = Set.union cols1 cols2

        if Set.count cols1 = 2 && Set.count cols2 = 2 && Set.count cols = 2 then
            let row1Cells = Set.map (fun col -> { Cell.col = col; row = row1 }) cols
            let row2Cells = Set.map (fun col -> { Cell.col = col; row = row2 }) cols
            let pointerCells = Set.union row1Cells row2Cells

            let secondaryHouses = Set.map Column cols

            makeHints pointerCells secondaryHouses

        else None

    | Column col1, Column col2 ->
        let rows1 = Set.map (fun (_, cell) -> cell.row) hht1
        let rows2 = Set.map (fun (_, cell) -> cell.row) hht2

        let rows = Set.union rows1 rows2

        if Set.count rows1 = 2 && Set.count rows2 = 2 && Set.count rows = 2 then
            let col1Cells = Set.map (fun row -> { Cell.col = col1; row = row }) rows
            let col2Cells = Set.map (fun row -> { Cell.col = col2; row = row }) rows
            let pointerCells = Set.union col1Cells col2Cells

            let secondaryHouses = Set.map Row rows

            makeHints pointerCells secondaryHouses

        else None
    | _ -> None

let xWingsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house1 : House) (house2 : House) = 
    let cells1 = houseCells house1

    let candidateCells1 = Set.map (fun cell -> ((candidateLookup cell), cell)) cells1

    let houseCandidates1 = Set.map fst candidateCells1 |> Set.unionMany

    let cells2 = houseCells house2

    let candidateCells2 = Set.map (fun cell -> ((candidateLookup cell), cell)) cells2

    let houseCandidates2 = Set.map fst candidateCells2 |> Set.unionMany

    let commonHouseCandidates = Set.intersect houseCandidates1 houseCandidates2

    Seq.map (xWingsPerHouseCandidate candidateLookup houseCells house1 house2 candidateCells1 candidateCells2) commonHouseCandidates

let xWingFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    let rows = 
        List.filter (fun house -> 
            match house with
            | Row _ -> true
            | _ -> false) houses
        |> List.toSeq

    let cols = 
        List.filter (fun house -> 
            match house with
            | Column _ -> true
            | _ -> false) houses
        |> List.toSeq

    let rowHints1 =
        Seq.mapi (fun i row1 ->
            Seq.mapi (fun j row2 -> xWingsPerHouse candidateLookup houseCells row1 row2) (Seq.skip (i + 1) rows)) rows

    let rowHints = 
        rowHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList

    let colHints1 =
        Seq.mapi (fun i col1 ->
            Seq.mapi (fun j col2 -> xWingsPerHouse candidateLookup houseCells col1 col2) (Seq.skip (i + 1) cols)) cols

    let colHints = 
        colHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList

    List.concat [ rowHints; colHints ]
