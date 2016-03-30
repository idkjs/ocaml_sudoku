open Sudoku
open Puzzlemap

open NUnit.Framework

let twoByFourPuzzleSpec =
    { size = 8;
      boxWidth = 2;
      boxHeight = 4;
      alphabet = 
            [1..8]
            |> List.map (fun i -> (char) i + '0' |> Digit)
            |> Digits.ofList }

let pick_some<'a> (as' : 'a list) : 'a list * 'a list =
    let picked =
        [9; 5; 2; 5; 5; 5; 1; 8; 9; 3; 5; 6]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    let expected =
        [1; 2; 3; 5; 6; 8; 9]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    (picked, expected)

let pick_more<'a> (as' : 'a list) : 'a list * 'a list =
    let picked =
        [9 * 8 + 1; 9 * 0 + 5; 9 * 2 + 4; 9 * 0 + 5; 9 * 0 + 5; 9 * 5 + 1; 9 * 0 + 8; 9 * 8 + 1; 9 * 3 + 3; 9 * 0 + 6]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    let expected =
        [9 * 0 + 5; 9 * 0 + 6; 9 * 0 + 8; 9 * 2 + 4; 9 * 3 + 3; 9 * 5 + 1; 9 * 8 + 1]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    (picked, expected)

[<Test>]
let ``Can make column sets``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (Columns.toList p.columns) in

    let picked' =
        picked
        |> Columns.ofList
        |> Columns.toList
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Columns.list_to_string expected, Columns.list_to_string picked')

[<Test>]
let ``Can make row sets``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (Rows.toList p.rows) in

    let picked' =
        picked
        |> Rows.ofList
        |> Rows.toList
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Rows.list_to_string expected, Rows.list_to_string picked')

[<Test>]
let ``Can make cell sets``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_more (Cells.toList p.cells) in

    let picked' =
        picked
        |> Cells.ofList
        |> Cells.toList
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Cells.list_to_string expected, Cells.list_to_string picked')

[<Test>]
let ``Can make digit sets``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (Digits.toList PuzzleShape.default'.alphabet) in

    let picked' =
        picked
        |> Digits.ofList
        |> Digits.toList
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Digits.list_to_string expected, Digits.list_to_string picked')

[<Test>]
let ``Can make columns``() =
    let p = tPuzzleMap PuzzleShape.default' in
    let actual = Columns.toList p.columns in

    let expected =
        [1..9]
        |> List.map CColumn
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Columns.list_to_string expected, Columns.list_to_string actual)

[<Test>]
let ``Can make rows``() = 
    let p = tPuzzleMap PuzzleShape.default' in
    let actual = Rows.toList p.rows in

    let expected =
        [1..9]
        |> List.map RRow
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.list_to_string expected, Rows.list_to_string actual)

[<Test>]
let ``Can make cells``() = 
    let p = tPuzzleMap PuzzleShape.default' in
    let actual = Cells.toList p.cells in

    let expected =
        [1..9]
        |> List.map
            (fun r ->
                [1..9]
                |> List.map
                    (fun c -> Cell.make (c |> CColumn) (r |> RRow)))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.list_to_string expected, Cells.list_to_string actual)

[<Test>]
let ``Can make stacks``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.stacks in

    let expected =
        [1..4]
        |> List.map SStack
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.list_to_string expected, Stacks.list_to_string actual)

[<Test>]
let ``Can make bands``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.bands in

    let expected =
        [1..2]
        |> List.map BBand
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.list_to_string expected, Bands.list_to_string actual)

[<Test>]
let ``Can make boxes``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.boxes in

    let expected =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> Box.make (s |> SStack) (b |> BBand)))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Boxes.list_to_string expected, Boxes.list_to_string actual)

[<Test>]
let ``Can make houses``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.houses in

    let expectedColumns =
        [1..8]
        |> List.map CColumn
        |> List.map HColumn
        in

    let expectedRows =
        [1..8]
        |> List.map RRow
        |> List.map HRow
        in

    let expectedBoxes =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> Box.make (s |> SStack) (b |> BBand)))
        |> List.concat
        |> List.map HBox
        in

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Houses.list_to_string expected, Houses.list_to_string actual)

[<Test>]
let ``Get column cells``() = 
    let p = tPuzzleMap PuzzleShape.default' in

    let column = CColumn 2 in

    let actual =
        Smap.get p.columnCells column
        |> Cells.toList in

    let expected =
        [1..9]
        |> List.map
            (fun r -> Cell.make column (r |> RRow))
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.list_to_string expected, Cells.list_to_string actual)

[<Test>]
let ``Get row cells``() = 
    let p = tPuzzleMap PuzzleShape.default' in

    let row = RRow 7 in

    let actual =
        Smap.get p.rowCells row
        |> Cells.toList in

    let expected =
        [1..9]
        |> List.map
            (fun c -> Cell.make (c |> CColumn) (7 |> RRow))
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.list_to_string expected, Cells.list_to_string actual)

[<Test>]
let ``Get stack for a column``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let actual =
        p.columns
        |> Columns.map (Smap.get p.columnStack)
        in

    let expected =
        [1..3]
        |> List.map
            (fun s ->
                [1..3]
                |> List.map (fun _ -> s |> SStack))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.list_to_string expected, Stacks.list_to_string actual)

[<Test>]
let ``Get stack columns``() = 
    let p = tPuzzleMap PuzzleShape.default' in

    let actual = Smap.get p.stackColumns (2 |> SStack) in

    let expected =
        [4..6]
        |> List.map CColumn
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Columns.list_to_string expected, Columns.list_to_string actual)

[<Test>]
let ``Get band for a row``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let actual =
        p.rows
        |> Rows.map (Smap.get p.rowBand)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map (fun _ -> b |> BBand))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.list_to_string expected, Bands.list_to_string actual)

[<Test>]
let ``Get band rows``() = 
    let p = tPuzzleMap PuzzleShape.default' in

    let actual = Smap.get p.bandRows (2 |> BBand) in

    let expected =
        [4..6]
        |> List.map RRow
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.list_to_string expected, Rows.list_to_string actual)

[<Test>]
let ``Get box for a cell``() =
    let p = tPuzzleMap PuzzleShape.default' in

    let actual =
        [1..9]
        |> List.map
            (fun r -> Cell.make (5 |> CColumn) (r |> RRow))
        |> List.map (Smap.get p.cellBox)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map
                    (fun _ -> Box.make (2 |> SStack) (b |> BBand)))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Boxes.list_to_string expected, Boxes.list_to_string actual)

let all_tests =
    [
    ``Can make column sets``;
    ``Can make row sets``;
    ``Can make cell sets``;
    ``Can make digit sets``;
    ``Can make columns``;
    ``Can make rows``;
    ``Can make cells``;
    ``Can make stacks``;
    ``Can make bands``;
    ``Can make boxes``;
    ``Can make houses``;
    ``Get column cells``;
    ``Get row cells``;
    ``Get stack for a column``;
    ``Get stack columns``;
    ``Get band for a row``;
    ``Get band rows``;
    ``Get box for a cell``;
    ]
